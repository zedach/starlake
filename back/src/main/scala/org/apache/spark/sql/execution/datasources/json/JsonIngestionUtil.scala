/**
  * Code in this file contains the almost exact same code that can be found in Apache Spark for parsing complex json schema
  *
  * Copyright 2014 and onwards The Apache Software Foundation.
  *
  * This product includes software developed at
  * The Apache Software Foundation (http://www.apache.org/).
  *
  *
  * Export Control Notice
  * ---------------------
  *
  * This distribution includes cryptographic software. The country in which you currently reside may have
  * restrictions on the import, possession, use, and/or re-export to another country, of encryption software.
  * BEFORE using any encryption software, please check your country's laws, regulations and policies concerning
  * the import, possession, or use, and re-export of encryption software, to see if this is permitted. See
  * <http://www.wassenaar.org/> for more information.
  *
  * The U.S. Government Department of Commerce, Bureau of Industry and Security (BIS), has classified this
  * software as Export Commodity Control Number (ECCN) 5D002.C.1, which includes information security software
  * using or performing cryptographic functions with asymmetric algorithms. The form and manner of this Apache
  * Software Foundation distribution makes it eligible for export under the License Exception ENC Technology
  * Software Unrestricted (TSU) exception (see the BIS Export Administration Regulations, Section 740.13) for
  * both object code and source code.
  *
  * The following provides more details on the included cryptographic software:
  *
  * This software uses Apache Commons Crypto (https://commons.apache.org/proper/commons-crypto/) to
  * support authentication, and encryption and decryption of data sent across the network between
  * services.
  */
package org.apache.spark.sql.execution.datasources.json

import java.util.Comparator

import com.ebiznext.comet.utils.Utils
import com.fasterxml.jackson.core.JsonToken._
import com.fasterxml.jackson.core.{JsonFactory, JsonParser}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.Row
import org.apache.spark.sql.catalyst.analysis.TypeCoercion
import org.apache.spark.sql.catalyst.json.JacksonUtils
import org.apache.spark.sql.types._

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
  * Code here comes from org.apache.spark.sql.execution.datasources.json.InferSchema
  *
  *
  */
object JsonIngestionUtil {
  private[this] val structFieldComparator = new Comparator[StructField] {
    override def compare(o1: StructField, o2: StructField): Int = {
      o1.name.compare(o2.name)
    }
  }


  def compareTypes(schemaType: DataType, datasetType: DataType): List[String] = {
    compareTypes(Nil, ("root", schemaType, true), ("root", datasetType, true))
  }

  /**
    * similar to compatibleType(...) but instead of creating a new datatype, simply check the compatibility
    * @param context : full path to attribute, makes error messages more understandable
    * @param schemaType
    * @param datasetType
    * @return List of errors, Nil when datasetType is compatible with schemaType
    */
  def compareTypes(context: List[String], schemaType: (String, DataType, Boolean), datasetType: (String, DataType, Boolean)): List[String] = {
    val schemaTypeNullable: Boolean = schemaType._3
    (schemaType._2, datasetType._2) match {
      case (t1, t2) if t1 == t2 => Nil
      case (_, NullType) if schemaTypeNullable => Nil
      case (_: IntegralType, _: IntegralType) => Nil
      case (_: FractionalType, _: IntegralType) => Nil
      case (_: FractionalType, _: FractionalType) => Nil
      case (_: TimestampType, _: DateType) | (_: DateType, _: TimestampType) => Nil
      case (_: StringType, _: StringType) => Nil
      case (StructType(unsortedFields1), StructType(unsortedFields2)) =>
        val fields1 = unsortedFields1.sortBy(_.name)
        val fields2 = unsortedFields2.sortBy(_.name)
        val errorList: mutable.MutableList[String] = mutable.MutableList.empty
        var f1Idx = 0
        var f2Idx = 0
        var typeComp = true
        while (f1Idx < fields1.length && f2Idx < fields2.length && typeComp) {
          val f1 = fields1(f1Idx)
          val f2 = fields2(f2Idx)
          val nameComp = f1.name.compareTo(f2.name)
          if (nameComp < 0 && f1.nullable) {
            // Field exists in schema is name and is not present in the message
            // go get the next field in the schema
            f1Idx += 1
          }
          else if (nameComp == 0) {
            // field is present in the schema and the dataset : check that types are equal
            val f1Type = f1.dataType
            val f2Type = f2.dataType
            errorList ++= compareTypes(context :+ schemaType._1, (f1.name, f1Type, f1.nullable), (f2.name, f2Type, f2.nullable))
            f1Idx += 1
            f2Idx += 1
            typeComp = typeComp && errorList.isEmpty
          }
          else {
            // Field is present in the message but not in the schema.
            errorList += s"unkonwn field ${f2.name} : ${f2.dataType.typeName} in context ${context.mkString(".")}"
            typeComp = false
          }
        }
        while (f2Idx < fields2.length) {
          val f2 = fields2(f2Idx)
          errorList += s"unkonwn field ${f2.name} : ${f2.dataType.typeName} in context ${context.mkString(".")}"
          f2Idx += 1
        }

        errorList.toList
      case (ArrayType(elementType1, containsNull1), ArrayType(elementType2, _)) =>
        compareTypes(context :+ schemaType._1, (schemaType._1, elementType1, containsNull1), (schemaType._1, elementType2, containsNull1))
      case (_, _) =>
        List(s"Validation error in context: ${context.mkString(".")}, ${datasetType._1}:${datasetType._2} isnullable:${datasetType._3} against " +
          s"schema ${schemaType._1}:${schemaType._2} isnullable:${schemaType._3}")
    }
  }


  def compatibleType(t1: DataType, t2: DataType): DataType = {
    TypeCoercion.findTightestCommonTypeOfTwo(t1, t2).getOrElse {
      (t1, t2) match {
        case (DoubleType, _: DecimalType) | (_: DecimalType, DoubleType) =>
          DoubleType

        case (t1: DecimalType, t2: DecimalType) =>
          val scale = math.max(t1.scale, t2.scale)
          val range = math.max(t1.precision - t1.scale, t2.precision - t2.scale)
          if (range + scale > 38) {
            // DecimalType can't support precision > 38
            DoubleType
          } else {
            DecimalType(range + scale, scale)
          }

        case (StructType(fields1), StructType(fields2)) =>
          val newFields = new java.util.ArrayList[StructField]()
          var f1Idx = 0
          var f2Idx = 0

          while (f1Idx < fields1.length && f2Idx < fields2.length) {
            val f1Name = fields1(f1Idx).name
            val f2Name = fields2(f2Idx).name
            val comp = f1Name.compareTo(f2Name)
            if (comp == 0) {
              val dataType = compatibleType(fields1(f1Idx).dataType, fields2(f2Idx).dataType)
              newFields.add(StructField(f1Name, dataType, nullable = true))
              f1Idx += 1
              f2Idx += 1
            } else if (comp < 0) { // f1Name < f2Name
              newFields.add(fields1(f1Idx))
              f1Idx += 1
            } else { // f1Name > f2Name
              newFields.add(fields2(f2Idx))
              f2Idx += 1
            }
          }
          while (f1Idx < fields1.length) {
            newFields.add(fields1(f1Idx))
            f1Idx += 1
          }
          while (f2Idx < fields2.length) {
            newFields.add(fields2(f2Idx))
            f2Idx += 1
          }
          StructType(newFields.toArray(Array.empty[StructField]))

        case (ArrayType(elementType1, containsNull1), ArrayType(elementType2, containsNull2)) =>
          ArrayType(compatibleType(elementType1, elementType2), containsNull1 || containsNull2)

        // The case that given `DecimalType` is capable of given `IntegralType` is handled in
        // `findTightestCommonTypeOfTwo`. Both cases below will be executed only when
        // the given `DecimalType` is not capable of the given `IntegralType`.
        case (t1: IntegralType, t2: DecimalType) =>
          compatibleType(DecimalType.forType(t1), t2)
        case (t1: DecimalType, t2: IntegralType) =>
          compatibleType(t1, DecimalType.forType(t2))

        // strings and every string is a Json object.
        case (_, _) => StringType
      }
    }
  }

  /**
    * Convert NullType to StringType and remove StructTypes with no fields
    */
  private def canonicalizeType(tpe: DataType): Option[DataType] = tpe match {
    case at@ArrayType(elementType, _) =>
      for {
        canonicalType <- canonicalizeType(elementType)
      } yield {
        at.copy(canonicalType)
      }

    case StructType(fields) =>
      val canonicalFields: Array[StructField] = for {
        field <- fields
        if field.name.length > 0
        canonicalType <- canonicalizeType(field.dataType)
      } yield {
        field.copy(dataType = canonicalType)
      }

      if (canonicalFields.length > 0) {
        Some(StructType(canonicalFields))
      } else {
        // per SPARK-8093: empty structs should be deleted
        None
      }

    case NullType => Some(StringType)
    case other => Some(other)
  }

  private def withCorruptField(
                                struct: StructType,
                                columnNameOfCorruptRecords: String): StructType = {
    if (!struct.fieldNames.contains(columnNameOfCorruptRecords)) {
      // If this given struct does not have a column used for corrupt records,
      // add this field.
      val newFields: Array[StructField] =
      StructField(columnNameOfCorruptRecords, StringType, nullable = true) +: struct.fields
      // Note: other code relies on this sorting for correctness, so don't remove it!
      java.util.Arrays.sort(newFields, structFieldComparator)
      StructType(newFields)
    } else {
      // Otherwise, just return this struct.
      struct
    }
  }

  /**
    * Remove top-level ArrayType wrappers and merge the remaining schemas
    */
  private def compatibleRootType(
                                  columnNameOfCorruptRecords: String,
                                  shouldHandleCorruptRecord: Boolean): (DataType, DataType) => DataType = {
    // Since we support array of json objects at the top level,
    // we need to check the element type and find the root level data type.
    case (ArrayType(ty1, _), ty2) =>
      compatibleRootType(columnNameOfCorruptRecords, shouldHandleCorruptRecord)(ty1, ty2)
    case (ty1, ArrayType(ty2, _)) =>
      compatibleRootType(columnNameOfCorruptRecords, shouldHandleCorruptRecord)(ty1, ty2)
    // If we see any other data type at the root level, we get records that cannot be
    // parsed. So, we use the struct as the data type and add the corrupt field to the schema.
    case (struct: StructType, NullType) => struct
    case (NullType, struct: StructType) => struct
    case (struct: StructType, o) if !o.isInstanceOf[StructType] && shouldHandleCorruptRecord =>
      withCorruptField(struct, columnNameOfCorruptRecords)
    case (o, struct: StructType) if !o.isInstanceOf[StructType] && shouldHandleCorruptRecord =>
      withCorruptField(struct, columnNameOfCorruptRecords)
    // If we get anything else, we call compatibleType.
    // Usually, when we reach here, ty1 and ty2 are two StructTypes.
    case (ty1, ty2) => compatibleType(ty1, ty2)
  }

  private[this] val emptyStructFieldArray = Array.empty[StructField]

  def inferSchema(parser: JsonParser): DataType = {
    parser.getCurrentToken match {
      case null | VALUE_NULL => NullType
      case FIELD_NAME =>
        parser.nextToken()
        inferSchema(parser)
      case START_ARRAY =>
        var elementType: DataType = NullType
        while (JacksonUtils.nextUntil(parser, END_ARRAY)) {
          elementType = compatibleType(elementType, inferSchema(parser))
        }
        ArrayType(elementType)
      case START_OBJECT =>
        val builder = Array.newBuilder[StructField]
        while (JacksonUtils.nextUntil(parser, END_OBJECT)) {
          builder += StructField(parser.getCurrentName, inferSchema(parser), nullable = true)
        }
        val fields: Array[StructField] = builder.result()
        StructType(fields)
      case VALUE_STRING =>
        StringType
      case VALUE_NUMBER_INT | VALUE_NUMBER_FLOAT =>
        import JsonParser.NumberType._
        parser.getNumberType match {
          case INT | LONG =>
            LongType
          case BIG_INTEGER | BIG_DECIMAL =>
            val v = parser.getDecimalValue
            if (Math.max(v.precision(), v.scale()) <= DecimalType.MAX_PRECISION) {
              DecimalType(Math.max(v.precision(), v.scale()), v.scale())
            } else {
              DoubleType
            }
          case FLOAT | DOUBLE =>
            DoubleType
        }
      case VALUE_TRUE | VALUE_FALSE =>
        BooleanType
      case _ =>
        throw new Exception("Should never happen")
    }

  }

  def parseString(content: String): Try[DataType] = {
    Try {
      Utils.withResources(factory.createParser(content)) { parser =>
        parser.nextToken()
        JsonIngestionUtil.inferSchema(parser)
      }
    }
  }

  def parseRDD(inputRDD: RDD[Row], schemaSparkType: DataType): RDD[Either[List[String], String]] = {
    inputRDD.mapPartitions { partition =>
      partition.map { row =>
        val rowAsString = row.getAs[String](0)
        parseString(rowAsString) match {
          case Success(datasetType) =>
            val errorList = compareTypes(schemaSparkType, datasetType)
            if (errorList.isEmpty)
              Right(rowAsString)
            else
              Left(errorList)

          case Failure(exception) =>
            Left(List(exception.toString))
        }
      }
    }
  }

  val factory = new JsonFactory()

}