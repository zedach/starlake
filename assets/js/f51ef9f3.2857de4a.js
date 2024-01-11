"use strict";(self.webpackChunkstarlake=self.webpackChunkstarlake||[]).push([[357],{3905:(e,a,t)=>{t.d(a,{Zo:()=>u,kt:()=>k});var n=t(67294);function l(e,a,t){return a in e?Object.defineProperty(e,a,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[a]=t,e}function r(e,a){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);a&&(n=n.filter((function(a){return Object.getOwnPropertyDescriptor(e,a).enumerable}))),t.push.apply(t,n)}return t}function i(e){for(var a=1;a<arguments.length;a++){var t=null!=arguments[a]?arguments[a]:{};a%2?r(Object(t),!0).forEach((function(a){l(e,a,t[a])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):r(Object(t)).forEach((function(a){Object.defineProperty(e,a,Object.getOwnPropertyDescriptor(t,a))}))}return e}function s(e,a){if(null==e)return{};var t,n,l=function(e,a){if(null==e)return{};var t,n,l={},r=Object.keys(e);for(n=0;n<r.length;n++)t=r[n],a.indexOf(t)>=0||(l[t]=e[t]);return l}(e,a);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(n=0;n<r.length;n++)t=r[n],a.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(l[t]=e[t])}return l}var o=n.createContext({}),p=function(e){var a=n.useContext(o),t=a;return e&&(t="function"==typeof e?e(a):i(i({},a),e)),t},u=function(e){var a=p(e.components);return n.createElement(o.Provider,{value:a},e.children)},c="mdxType",m={inlineCode:"code",wrapper:function(e){var a=e.children;return n.createElement(n.Fragment,{},a)}},d=n.forwardRef((function(e,a){var t=e.components,l=e.mdxType,r=e.originalType,o=e.parentName,u=s(e,["components","mdxType","originalType","parentName"]),c=p(t),d=l,k=c["".concat(o,".").concat(d)]||c[d]||m[d]||r;return t?n.createElement(k,i(i({ref:a},u),{},{components:t})):n.createElement(k,i({ref:a},u))}));function k(e,a){var t=arguments,l=a&&a.mdxType;if("string"==typeof e||l){var r=t.length,i=new Array(r);i[0]=d;var s={};for(var o in a)hasOwnProperty.call(a,o)&&(s[o]=a[o]);s.originalType=e,s[c]="string"==typeof e?e:l,i[1]=s;for(var p=2;p<r;p++)i[p]=t[p];return n.createElement.apply(null,i)}return n.createElement.apply(null,t)}d.displayName="MDXCreateElement"},23855:(e,a,t)=>{t.r(a),t.d(a,{assets:()=>o,contentTitle:()=>i,default:()=>m,frontMatter:()=>r,metadata:()=>s,toc:()=>p});var n=t(87462),l=(t(67294),t(3905));const r={sidebar_position:20,title:"Load"},i=void 0,s={unversionedId:"reference/load",id:"version-0.8.0/reference/load",title:"Load",description:"Domain Configuration file",source:"@site/versioned_docs/version-0.8.0/reference/20.load.md",sourceDirName:"reference",slug:"/reference/load",permalink:"/starlake/docs/0.8.0/reference/load",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/versioned_docs/version-0.8.0/reference/20.load.md",tags:[],version:"0.8.0",sidebarPosition:20,frontMatter:{sidebar_position:20,title:"Load"},sidebar:"starlakeSidebar",previous:{title:"Extract",permalink:"/starlake/docs/0.8.0/reference/extract"},next:{title:"Transform",permalink:"/starlake/docs/0.8.0/reference/transform"}},o={},p=[{value:"Domain Configuration file",id:"domain-configuration-file",level:2},{value:"Properties",id:"properties",level:2},{value:"Table",id:"table",level:2},{value:"Metadata",id:"metadata",level:2},{value:"Attribute",id:"attribute",level:2},{value:"Connections",id:"connections",level:2},{value:"BigQuerySink",id:"bigquerysink",level:3},{value:"EsSink",id:"essink",level:3},{value:"JdbcSink",id:"jdbcsink",level:3},{value:"KafkaSink",id:"kafkasink",level:2},{value:"RowLevelSecurity",id:"rowlevelsecurity",level:2},{value:"AccessControlEntry",id:"accesscontrolentry",level:2}],u={toc:p},c="wrapper";function m(e){let{components:a,...t}=e;return(0,l.kt)(c,(0,n.Z)({},u,t,{components:a,mdxType:"MDXLayout"}),(0,l.kt)("h2",{id:"domain-configuration-file"},"Domain Configuration file"),(0,l.kt)("p",null,"Let's say you are willing to import ",(0,l.kt)("inlineCode",{parentName:"p"},"customers")," and ",(0,l.kt)("inlineCode",{parentName:"p"},"orders")," from your ",(0,l.kt)("inlineCode",{parentName:"p"},"sales")," system.\n",(0,l.kt)("inlineCode",{parentName:"p"},"sales")," is therefore the domain and ",(0,l.kt)("inlineCode",{parentName:"p"},"customers")," & ",(0,l.kt)("inlineCode",{parentName:"p"},"orders")," are your tables."),(0,l.kt)("ul",null,(0,l.kt)("li",{parentName:"ul"},"In a DBMS, a Domain would be implemented by a DBMS schema  and a table by a DBMS table."),(0,l.kt)("li",{parentName:"ul"},"In BigQuery, the domain name would be the Big Query dataset name, and the table would be implemented by a Big Query table.")),(0,l.kt)("p",null,"The Domain configuration file is stored in a file named ",(0,l.kt)("inlineCode",{parentName:"p"},"_config.sl.yml")," in the domain folder."),(0,l.kt)("h2",{id:"properties"},"Properties"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"name: String\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". Domain name (default to folder name). Make sure you use a name that may be used as a folder name on the target storage."),(0,l.kt)("ul",null,(0,l.kt)("li",{parentName:"ul"},"When using HDFS or Cloud Storage,  files once ingested are stored in a sub-directory of a directory named after the domain name."),(0,l.kt)("li",{parentName:"ul"},"When used with BigQuery, files are ingested and sorted in tables under a dataset named after the domain name.")),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"directory: String\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Required"),'. Folder on the local filesystem where incoming files are stored.\nTypically, this folder will be scanned periodically to move the files to the cluster for ingestion.\nFiles located in this folder are moved to the pending folder for ingestion by the "import" command.'),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"metadata: Metadata\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". Default Schema metadata.\nThis metadata is applied to the tables defined in this domain.\nMetadata properties may be redefined at the table level.\nSee Metadata Entity for more details."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"comment: String\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". Domain Description (free text)"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"tags: Set[String]\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),'.\nList of key value pairs to set as database properties (or Dataset properties in  BigQuery).\nEach string holds data in the form "key=value" or "key" if there is no value.'),(0,l.kt)("h2",{id:"table"},"Table"),(0,l.kt)("p",null,"A domain usually contains multiple tables. Each table define how the contents of the input file should be parsed.\nThe table configuration file is stored in a file named ",(0,l.kt)("inlineCode",{parentName:"p"},"_tablename.sl.yml")," in the domain folder."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"name: String\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". Table name, default to the filename prefix. Must be unique among all the tables belonging to the same domain.\nWill become a hive table name On Premise or BigQuery table name on GCP."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"pattern: String\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Required"),". Filename pattern to which this schema must be applied. This may be any ",(0,l.kt)("a",{parentName:"p",href:"https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html"},"Java Regex")),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"primaryKey: List[String]\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". List of attributes that make up the primary key of this table.\nOn some database, this setting is just for documentation purpose.\nThis is also used for graphviz generation."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"acl: List[AccessControlEntry]\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),".\nList of access control entries to limit access to the resulting table."),(0,l.kt)("p",null,"This execute a set of grants on  the table.\nSee ",(0,l.kt)("a",{parentName:"p",href:"#accesscontrolentry"},"Access Control Entry")," for more details"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"attributes: List[Attribute]\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Required"),". Attributes parsing rules.\nSee ",(0,l.kt)("a",{parentName:"p",href:"#attribute"},"Attributes")," for more details."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"metadata: Metadata\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". Dataset metadata\nSee ",(0,l.kt)("a",{parentName:"p",href:"#metadata"},"Metadata")," for more details."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"comment: String\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". Free text that will be added as the table description"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"presql: String\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),".\nSQL requests to execute before starting the ingestion process.\nYou may create view that is used in a UDF during the ingestion process for example."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"postsql: String\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),".\nSQL requests to execute post execution of te ingestion process.\nYou may for example set the table ownership in this section."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"tags: Set[String]\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),'.\nList of key value pairs to set as table properties.\nEach string holds data in the form "key=value" or "key" if there is no value.'),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"rls: List[RowLevelSecurity]\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". Row level security to apply to this schema once it is ingested.\nThis usually execute a set on grants by applying a predicate filter to restrict\naccess to a subset of the rows in the table.\nSee ",(0,l.kt)("a",{parentName:"p",href:"#rowlevelsecurity"},"Row Level Security")," for more details"),(0,l.kt)("h2",{id:"metadata"},"Metadata"),(0,l.kt)("p",null,"Specify table properties.\nThese properties may be specified at the table or domain level\nAny property not specified at the table level is taken from the\none specified at the domain level or else the default value is returned."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"mode: Enum\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". FILE mode by default. FILE and STREAM are the two accepted values. "),(0,l.kt)("ul",null,(0,l.kt)("li",{parentName:"ul"},"FILE when loading files."),(0,l.kt)("li",{parentName:"ul"},"STREAM when streaming data from Kafka.")),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"format: Enum\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". DSV by default. Supported file formats are:"),(0,l.kt)("ul",null,(0,l.kt)("li",{parentName:"ul"},'DSV: Delimiter-separated values file. Delimiter value iss specified in the "separator" field.'),(0,l.kt)("li",{parentName:"ul"},"POSITION: FIXED format file where values are located at an exact position in each line."),(0,l.kt)("li",{parentName:"ul"},"SIMPLE_JSON: For optimisation purpose, we differentiate flat JSON from JSON with deep level fields. SIMPLE_JSON are JSON files with top level fields only."),(0,l.kt)("li",{parentName:"ul"},"JSON:  Deep JSON file. Use only when your json documents contain subdocuments, otherwise prefer to use SIMPLE_JSON since it is much faster."),(0,l.kt)("li",{parentName:"ul"},"XML: For XML files")),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"encoding: String\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". UTF-8 if not specified."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"multiline: Boolean\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". Are json objects on a single line or multiple line ? Single by default.  false means single. false also means faster"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"array: Boolean\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". Is the json stored as a single object array ? false by default. This means that by default we have on json document per line."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"withHeader: Boolean\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". When the input file is in the DSV file format, does the dataset has a header ? true by default"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"separator: String\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". When the input file is in the DSV file format, this field contains the values delimiter,  ';' by default value may also be a multichar string."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"quote: Char\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". The String quote char, '\"' by default"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"escape: Char\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". Escaping char '\\' by default"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"write: Enum\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". Write mode, APPEND by default"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"partition: List[String]\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". Partition columns, no partitioning by default"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"sink: Sink\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". Should the dataset be indexed to this sink ?\nSee ",(0,l.kt)("a",{parentName:"p",href:"#sink"},"Sink")," for more details."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"ignore: String\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". Pattern to ignore or UDF to apply to ignore some lines"),(0,l.kt)("h2",{id:"attribute"},"Attribute"),(0,l.kt)("p",null,'A field in the schema. For struct fields, the field "attributes" contains all sub attributes'),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"name: String\n")),(0,l.kt)("p",null,"Attribute name as defined in the source and as received in the file"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"type: String\n")),(0,l.kt)("p",null,"Semantic type of the attribute."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"array: Boolean\n")),(0,l.kt)("p",null,"Is it an array ?"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"required: Boolean\n")),(0,l.kt)("p",null,"Should this attribute always be present in the source"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"privacy:PrivacyLevel\n")),(0,l.kt)("p",null,"Should this attribute be applied a privacy transformation at ingestion time"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"comment: String\n")),(0,l.kt)("p",null,"free text for attribute description"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"rename: String\n")),(0,l.kt)("p",null,"If present, the attribute is renamed with this name"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"metricType:MetricType\n")),(0,l.kt)("p",null,"If present, what kind of stat should be computed for this field"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"attributes: List[Attribute]\n")),(0,l.kt)("p",null,"List of sub-attributes (valid for JSON and XML files only)"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"position: Position\n")),(0,l.kt)("p",null,"Valid only when file format is POSITION"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"default: String\n")),(0,l.kt)("p",null,"Default value for this attribute when it is not present."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"foreignKey: String\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Optional"),". reference to a column or a table in the format ","[domain.]","table","[.attribute]",".\nUsed for documentation purpose and graphviz generation. ."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"\n\n````scala\ntags:Set[String]\n")),(0,l.kt)("p",null,"Tags associated with this attribute"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"trim: Trim\n")),(0,l.kt)("p",null,"Should we trim the attribute value ?"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"script: String\n")),(0,l.kt)("p",null,"Scripted field: SQL request on renamed column"),(0,l.kt)("h2",{id:"connections"},"Connections"),(0,l.kt)("p",null,"Transformations and loading happen through a connection to a database.\nConnections are defined in the ",(0,l.kt)("inlineCode",{parentName:"p"},"metadata/connections.sl.yml")," or in the ",(0,l.kt)("inlineCode",{parentName:"p"},"connections:")," section in the ",(0,l.kt)("inlineCode",{parentName:"p"},"metadata/application.sl.yml"),"  file.\nThe connection decribes the technical details to connect to the database."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"type: String\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"required"),". may be one of the following values:"),(0,l.kt)("ul",null,(0,l.kt)("li",{parentName:"ul"},"FS | FILESYSTEM: Filesystem connection used to read and write files in parquet format on the local filesystem/HDFS/S3/ADFS/GCS."),(0,l.kt)("li",{parentName:"ul"},"BQ | BIGQUERY: BigQuery connection used to read and write data from/to BigQuery."),(0,l.kt)("li",{parentName:"ul"},"DATABRICKS | SPARK: Spark connection used to read and write data from/to Spark."),(0,l.kt)("li",{parentName:"ul"},"HIVE: Like FS but will also create tables in the Hive metastore."),(0,l.kt)("li",{parentName:"ul"},"JDBC: JDBC connection used to read and write data from/to a JDBC compliant database."),(0,l.kt)("li",{parentName:"ul"},"ES: Elasticsearch connection used to read and write data from/to Elasticsearch."),(0,l.kt)("li",{parentName:"ul"},"KAFKA: Kafka connection used to read and write data from/to Kafka.")),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"\n\n\n## Sink\nOnce ingested, files may be sinked to BigQuery, Kafka, Elasticsearch or any JDBC compliant Database.\n\n````scala\ntype: Enum\n")),(0,l.kt)("ul",null,(0,l.kt)("li",{parentName:"ul"},"JDBC: dataset will be sinked to a JDBC Database. See JdbcSink below"),(0,l.kt)("li",{parentName:"ul"},"ES: dataset is indexed into Elasticsearch. See EsSink below"),(0,l.kt)("li",{parentName:"ul"},"BQ: Dataset is sinked to BigQuery. See BigQuerySink below"),(0,l.kt)("li",{parentName:"ul"},"KAFKA: Dataset is sinked to a Kafka topic.  See KafkaSink below"),(0,l.kt)("li",{parentName:"ul"},"None: Don't sink. This is the default.")),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"name: String\n")),(0,l.kt)("p",null,"This optional name is used when the configuration is specified in the application.conf file instead of inline in the YAML file.\nThis is useful when the same sink parameters are used for different datasets."),(0,l.kt)("h3",{id:"bigquerysink"},"BigQuerySink"),(0,l.kt)("p",null,"When the sink type field is set to BQ, the options below should be provided."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"location: String\n")),(0,l.kt)("p",null,"Database location (EU, US, ...)"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"timestamp: String\n")),(0,l.kt)("p",null,"The timestamp column to use for table partitioning if any. No partitioning by default"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"clustering: List[String]\n")),(0,l.kt)("p",null,"List of ordered columns to use for table clustering"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"days: Int\n")),(0,l.kt)("p",null,"Number of days before this table is set as expired and deleted. Never by default."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"requirePartitionFilter: Boolean\n")),(0,l.kt)("p",null,"Should be require a partition filter on every request ? No by default."),(0,l.kt)("h3",{id:"essink"},"EsSink"),(0,l.kt)("p",null,"When the sink ",(0,l.kt)("em",{parentName:"p"},"type")," field is set to ES, the options below should be provided.\nElasticsearch options are specified in the application.conf file."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"id: String\n")),(0,l.kt)("p",null,"Attribute to use as id of the document. Generated by Elasticseach if not specified."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"timestamp: String\n")),(0,l.kt)("p",null,'Timestamp field format as expected by Elasticsearch ("{beginTs|yyyy.MM.dd}" for example).'),(0,l.kt)("h3",{id:"jdbcsink"},"JdbcSink"),(0,l.kt)("p",null,"When the sink ",(0,l.kt)("em",{parentName:"p"},"type")," field is set to JDBC, the options below should be provided."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"connection: String\n")),(0,l.kt)("p",null,"The JDBC Connection String. Specific to the target JDBC database"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"partitions: Int\n")),(0,l.kt)("p",null,"Number of Spark partitions"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"batchsize: Int\n")),(0,l.kt)("p",null,"Batch size of each JDBC bulk insert"),(0,l.kt)("h2",{id:"kafkasink"},"KafkaSink"),(0,l.kt)("p",null,"When the sink ",(0,l.kt)("em",{parentName:"p"},"type")," field is set to KAFKA, you just need to set the name field to configuration name defined in the application.conf.\nPlease refer to ",(0,l.kt)("a",{parentName:"p",href:"/starlake/docs/0.8.0/reference/configuration#kafka"},"this section")," to understand how kafka sink are defined in the configuration file."),(0,l.kt)("h2",{id:"rowlevelsecurity"},"RowLevelSecurity"),(0,l.kt)("p",null,"User / Group and Service accounts rights on a subset of the table."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"name: String\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Required"),". This Row Level Security unique name."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"predicate: String\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Required"),". The condition that goes to the WHERE clause and limit the visible rows."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"grants: List[String]\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Required"),". user / groups / service accounts to which this security level is applied."),(0,l.kt)("p",null,"Example"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-yaml"},"rls:\n  - name: business_dept\n    predicate: departement like 'Business'\n    grants:\n      - user:user@starlake.ai\n      - group:goup@starlake.ai\n      - serviceAccount:service@gserviceaccount.google.com\n")),(0,l.kt)("p",null,"The example above will:"),(0,l.kt)("ul",null,(0,l.kt)("li",{parentName:"ul"},"On BigQuery, set a row level security access rule"),(0,l.kt)("li",{parentName:"ul"},"Ignored on Hive"),(0,l.kt)("li",{parentName:"ul"},"On Databricks, create a view named business_dept as follows:")),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sql"},"create view business_dept as \nselect * \nfrom table_name\nwhere current_user() like 'user@starlake.ai' or is_member('somegroup')\n")),(0,l.kt)("h2",{id:"accesscontrolentry"},"AccessControlEntry"),(0,l.kt)("p",null,"List the user / groups /service account that should be assigned a role."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"role: String\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Required"),". This Row Level Security unique name."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"grants: List[String]\n")),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Required"),". user / groups / service accounts to which this access control is applied."),(0,l.kt)("p",null,"Databricks Example:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-yaml"},"acl:\n  role: SELECT\n  grants:\n    - user@starlake.ai\n    - group\n")),(0,l.kt)("p",null,"BigQuery Example"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-yaml"},"acl:\n  role: role/bigQueryViewer\n  grants:\n    - user:user@starlake.ai\n    - group:goup@starlake.ai\n    - serviceAccount:service@gserviceaccount.google.com\n")),(0,l.kt)("p",null,"Hive Example"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-yaml"},"acl:\n  role: SELECT\n  grants:\n    - user: someone\n    - group: onegroup\n")))}m.isMDXComponent=!0}}]);