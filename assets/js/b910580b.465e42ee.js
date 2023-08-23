"use strict";(self.webpackChunkstarlake_docs=self.webpackChunkstarlake_docs||[]).push([[5519],{3905:(e,t,a)=>{a.d(t,{Zo:()=>m,kt:()=>h});var n=a(7294);function i(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function l(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function r(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?l(Object(a),!0).forEach((function(t){i(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):l(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function o(e,t){if(null==e)return{};var a,n,i=function(e,t){if(null==e)return{};var a,n,i={},l=Object.keys(e);for(n=0;n<l.length;n++)a=l[n],t.indexOf(a)>=0||(i[a]=e[a]);return i}(e,t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(n=0;n<l.length;n++)a=l[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(i[a]=e[a])}return i}var s=n.createContext({}),d=function(e){var t=n.useContext(s),a=t;return e&&(a="function"==typeof e?e(t):r(r({},t),e)),a},m=function(e){var t=d(e.components);return n.createElement(s.Provider,{value:t},e.children)},p="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},c=n.forwardRef((function(e,t){var a=e.components,i=e.mdxType,l=e.originalType,s=e.parentName,m=o(e,["components","mdxType","originalType","parentName"]),p=d(a),c=i,h=p["".concat(s,".").concat(c)]||p[c]||u[c]||l;return a?n.createElement(h,r(r({ref:t},m),{},{components:a})):n.createElement(h,r({ref:t},m))}));function h(e,t){var a=arguments,i=t&&t.mdxType;if("string"==typeof e||i){var l=a.length,r=new Array(l);r[0]=c;var o={};for(var s in t)hasOwnProperty.call(t,s)&&(o[s]=t[s]);o.originalType=e,o[p]="string"==typeof e?e:i,r[1]=o;for(var d=2;d<l;d++)r[d]=a[d];return n.createElement.apply(null,r)}return n.createElement.apply(null,a)}c.displayName="MDXCreateElement"},5180:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>s,contentTitle:()=>r,default:()=>u,frontMatter:()=>l,metadata:()=>o,toc:()=>d});var n=a(7462),i=(a(7294),a(3905));const l={sidebar_position:200},r="Load",o={unversionedId:"userguide/load",id:"userguide/load",title:"Load",description:"Load Rules",source:"@site/docs/userguide/200.load.md",sourceDirName:"userguide",slug:"/userguide/load",permalink:"/starlake/docs/userguide/load",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/docs/userguide/200.load.md",tags:[],version:"current",sidebarPosition:200,frontMatter:{sidebar_position:200},sidebar:"starlakeSidebar",previous:{title:"Complete example",permalink:"/starlake/docs/userguide/example"},next:{title:"Transform",permalink:"/starlake/docs/userguide/transform"}},s={},d=[{value:"Load Rules",id:"load-rules",level:2},{value:"Type Rules",id:"type-rules",level:2},{value:"Domain Rules",id:"domain-rules",level:2},{value:"Schema Rules",id:"schema-rules",level:2},{value:"Privacy Strategy",id:"privacy-strategy",level:3},{value:"Write Strategy",id:"write-strategy",level:2},{value:"Partitioning",id:"partitioning",level:3},{value:"Compaction Strategy",id:"compaction-strategy",level:3},{value:"Solution 1 : Naive Compaction",id:"solution-1--naive-compaction",level:4},{value:"Solution 2 : Sampling",id:"solution-2--sampling",level:4},{value:"Solution 3 : Absolute Compaction",id:"solution-3--absolute-compaction",level:4},{value:"Load Workflow",id:"load-workflow",level:2},{value:"Import Step",id:"import-step",level:2},{value:"How it works",id:"how-it-works",level:3},{value:"Running it",id:"running-it",level:3},{value:"Load Step",id:"load-step",level:2},{value:"How it works",id:"how-it-works-1",level:3},{value:"Running it",id:"running-it-1",level:3},{value:"Ingestion Step",id:"ingestion-step",level:2},{value:"How it works",id:"how-it-works-2",level:3},{value:"Running it",id:"running-it-2",level:3},{value:"Export Step",id:"export-step",level:2},{value:"How it works",id:"how-it-works-3",level:3},{value:"Running it",id:"running-it-3",level:3}],m={toc:d},p="wrapper";function u(e){let{components:t,...a}=e;return(0,i.kt)(p,(0,n.Z)({},m,a,{components:t,mdxType:"MDXLayout"}),(0,i.kt)("h1",{id:"load"},"Load"),(0,i.kt)("h2",{id:"load-rules"},"Load Rules"),(0,i.kt)("p",null,"Load rules are stored in the folder SL_ROOT/metadata folder."),(0,i.kt)("admonition",{type:"note"},(0,i.kt)("p",{parentName:"admonition"},"You need to export the SL_ROOT variable before executing any comet load step.")),(0,i.kt)("p",null,(0,i.kt)("inlineCode",{parentName:"p"},"export SL_METADATA=hdfs:///my/metadata")),(0,i.kt)("p",null,"Dataset validation is based on a set of rules we define in schema files.\nSchema files describe how the input files are parsed using a set of rules :"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"Type Rules: Rules that describe the recognized fields formats."),(0,i.kt)("li",{parentName:"ul"},"Domain Rules: Rules that describe the file format and load strategy"),(0,i.kt)("li",{parentName:"ul"},"Schema Rules: Rules that describe field format using pattern matching"),(0,i.kt)("li",{parentName:"ul"},"Assertions:  Rules that must be respected by the whole input file. These rules are executed once the file has been ingested.")),(0,i.kt)("h2",{id:"type-rules"},"Type Rules"),(0,i.kt)("p",null,"Types are defined in the file $SL_ROOT/metadata/types/types.comet.yml."),(0,i.kt)("p",null,"A type is defined by:"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},'its name: a string such as "username", "int", "boolean", "long"')),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"the primitive type it is mapped to. Below is the list of all primitive types:"),(0,i.kt)("ul",{parentName:"li"},(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"string")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"byte"),": single char field"),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"decimal"),": For exact arithmetic. Used for money computation"),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"long"),": integers"),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"double"),": floating numbers"),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"boolean"),": boolean values"),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"date")," : date only fields"),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"timestamp"),": date time fields"))),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"the pattern it should match : A java pattern matching expression that matches the field"),(0,i.kt)("ul",{parentName:"li"},(0,i.kt)("li",{parentName:"ul"},'for types of primitive type "date" or date time, "epoch_milli", "epoch_second" or any predefined or custom date pattern as defined in the ',(0,i.kt)("a",{parentName:"li",href:"https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/format/DateTimeFormatter.html#BASIC_ISO_DATE"},"Date Time Formatter")," Specification.")))),(0,i.kt)("p",null,"For each primitive type, a type is defined by default. These default types are\nlocated in the file $SL_ROOT/metadata/types/default.comet.yml and they may be redefined\nin the file $SL_ROOT/metadata/types/types.comet.yml"),(0,i.kt)("p",null,"File ",(0,i.kt)("inlineCode",{parentName:"p"},"$SL_ROOT/metadata/types/default.comet.yml")),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-yaml"},'types:\n  - name: "string"\n    primitiveType: "string"\n    pattern: ".+"\n    sample: "Hello World"\n    comment: "Any set of chars"\n  - name: "byte"\n    primitiveType: "byte"\n    pattern: "."\n    sample: "x"\n    comment: "Any set of chars"\n  - name: "date"\n    primitiveType: "date"\n    pattern: "yyyy/MM/dd"\n    sample: "2018/07/21"\n    comment: "Data in the format yyyy/MM/dd"\n  - name: "double"\n    primitiveType: "double"\n    pattern: "-?\\\\d*\\\\.{0,1}\\\\d+"\n    sample: "-45.78"\n    comment: "Any floating value"\n  - name: "long"\n    primitiveType: "long"\n    pattern: "-?\\\\d+"\n    sample: "-64564"\n    comment: "any positive or negative number"\n  - name: "boolean"\n    primitiveType: "boolean"\n    pattern: "(?i)true|yes|[y1]<-TF->(?i)false|no|[n0]"\n    sample: "TruE"\n  - name: "timestamp"\n    primitiveType: "timestamp"\n    pattern: "epoch_milli"\n    sample: "1548165436433"\n    comment: "date/time in epoch millis"\n')),(0,i.kt)("p",null,'Any semantic type that maps to the boolean primitive type must match against a special regex.\nThis regex is made of two parts separated by the string "<-TF->". values matching the left side will\nbe interpreted as the boolean value "true" and values matching the right side will be interpreted as the boolean value "false".'),(0,i.kt)("p",null,"We may add new types that map to these primitive types.\nFor our example above, we will add the following\nsemantic types to allow better validation on the input fields"),(0,i.kt)("p",null,"$SL_ROOT/metadata/types/types.comet.yml"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-yaml"},'types:\n  - name: "email"\n    primitiveType: "string"\n    pattern: "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\\\\\\\.[A-Za-z]{2,6}"\n    sample: "me@company.com"\n    comment: "Valid email only"\n  - name: "customerid"\n    primitiveType: "string"\n    pattern: "[A-Z][0-9]{6}"\n    sample: "A123456"\n    comment: "Letter followed by 6 digits"\n  - name: "sellerid"\n    primitiveType: "string"\n    pattern: "[0-9]{6}"\n    sample: "123456"\n    comment: "6 digits string"\n')),(0,i.kt)("p",null,"Now that we have defined the set of semantic\ntypes we want to recognize, we may start defining our schemas."),(0,i.kt)("h2",{id:"domain-rules"},"Domain Rules"),(0,i.kt)("p",null,'Files are organized by domain. In our example, the "customers" and "orders"\nfiles belong to the "sales" domain  and the "sellers" file belong to the "HR"\ndomain.'),(0,i.kt)("p",null,"Domain rules are YAML files located in the folder\n$SL_ROOT/metadata/load. They defined :"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"The directory where the files coming from this domain are stored"),(0,i.kt)("li",{parentName:"ul"},'The ack extension for ack files. "ack" by default.'),(0,i.kt)("li",{parentName:"ul"},'Raw file extensions to recognize.  "json", "csv", "dsv", "psv" by default.')),(0,i.kt)("p",null,'The load pipeline also automatically recognize compressed files with\nthe extension "tgz", "gz" and "zip". These files are uncompressed in a\ntemporary location and each raw file in the archive is ingested\nif the filename matches a file pattern in one of the schema in the domain,\notherwise the file is moved to the "unsolved" folder under the domain name\nin the cluster.'),(0,i.kt)("p",null,"The file below explains it all:"),(0,i.kt)("p",null,"File ",(0,i.kt)("inlineCode",{parentName:"p"},"$SL_ROOT/metadata/load/sales/_config.comet.yml")),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-yaml"},'load:\n  name: "sales"\n  directory: "/mnt/incoming/sales"\n  ack: "ack"\n')),(0,i.kt)("p",null,'This instruct the Starlake Data Pipeline to scan the "/mnt/incoming/sales"\ndirectory '),(0,i.kt)("admonition",{type:"note"},(0,i.kt)("p",{parentName:"admonition"},"To process files without relying on ack files, simply omit or  define the ack attribute with an empty string :",(0,i.kt)("inlineCode",{parentName:"p"},'ack : ""'))),(0,i.kt)("p",null,"To ingest files present in the domain incoming directory (/mnt/incoming/sales),\nwe need to add schema definitions to the domain description file,\naka $SL_METADATA/domains/sales.yml."),(0,i.kt)("p",null,"You can define only one domain per YAML domain definition file."),(0,i.kt)("h2",{id:"schema-rules"},"Schema Rules"),(0,i.kt)("p",null,"A schema is associated to an incoming file if the filename matches the pattern\ndefined in the schema.\nThe schema hold the parsing rules through metadata describing the file format\nand type mapping rules for each attribute."),(0,i.kt)("p",null,'First, we add the schema definition to the "customer" file in the domain definition file'),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="Domain configuration file $SL_ROOT/metadata/load/sales/_config.comet.yml"',title:'"Domain',configuration:!0,file:!0,'$SL_ROOT/metadata/load/sales/_config.comet.yml"':!0},'load:\n  name: "sales"\n  directory: "/mnt/incoming/sales"\n  ack: "ack"\n')),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="Schema definition file $SL_ROOT/metadata/load/sales/customers.comet.yml"',title:'"Schema',definition:!0,file:!0,'$SL_ROOT/metadata/load/sales/customers.comet.yml"':!0},'table:\n  name: "customers"\n  pattern: "customers-.*.psv"\n  attributes:\n    - name: "id"\n      type: "customerid"\n      required: true\n    - name: "signup"\n      type: "datetime"\n      required: false\n    - name: "contact"\n      type: "email"\n      required: false\n    - name: "birthdate"\n      type: "date"\n      required: false\n    - name: "name1"\n      type: "string"\n      required: false\n      rename: "firstname"\n    - name: "name2"\n      type: "string"\n      required: false\n      rename: "lastname"\n  metadata:\n    mode: "FILE"\n    format: "DSV"\n    withHeader: true\n    separator: "|"\n    quote: "\\""\n    escape: "\\\\"\n    write: "APPEND"\n')),(0,i.kt)("p",null,"The schema section in the YAML above should be read as follows :"),(0,i.kt)("table",null,(0,i.kt)("thead",{parentName:"table"},(0,i.kt)("tr",{parentName:"thead"},(0,i.kt)("th",{parentName:"tr",align:null},"Schema"),(0,i.kt)("th",{parentName:"tr",align:null},"Definition"))),(0,i.kt)("tbody",{parentName:"table"},(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:null},"pattern"),(0,i.kt)("td",{parentName:"tr",align:null},"Filename pattern to match in the domain directory")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:null},"name"),(0,i.kt)("td",{parentName:"tr",align:null},"Schema name: folder where the dataset is stored and Hive table prefix.")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:null},"metadata.mode"),(0,i.kt)("td",{parentName:"tr",align:null},"always FILE. STREAM is reserved for future use.")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:null},"metadata.format"),(0,i.kt)("td",{parentName:"tr",align:null},"DSV for delimiter separated values file. SIMPLE_JSON and JSON are also supported.")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:null},"metadata.withHeader"),(0,i.kt)("td",{parentName:"tr",align:null},"Does the input file has a header")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:null},"metadata.separator"),(0,i.kt)("td",{parentName:"tr",align:null},"What is the field separator")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:null},"metadata.quote"),(0,i.kt)("td",{parentName:"tr",align:null},"How are string delimited")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:null},"metadata.escape"),(0,i.kt)("td",{parentName:"tr",align:null},"How are characters escaped")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:null},"metadata.write"),(0,i.kt)("td",{parentName:"tr",align:null},"Should we APPEND or OVERWRITE existing data in the  cluster")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:null},"metadata.multiline"),(0,i.kt)("td",{parentName:"tr",align:null},'"Are JSON object on multiple line. Used when format is JSON or SIMPLE_JSON. This slow down parsing"')),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:null},"metadata.array"),(0,i.kt)("td",{parentName:"tr",align:null},'"Should we treat the file as a single array of JSON objects. Used  when format is JSON or SIMPLE_JSON and the input data is in brackets ',"[...]",'"')))),(0,i.kt)("admonition",{type:"note"},(0,i.kt)("p",{parentName:"admonition"},"Simple JSON are flat JSON files. JSON may be used wherever\nyou use SIMPLE_JSON but SIMPLE_JSON will make parsing much faster.")),(0,i.kt)("p",null,"Metadata properties may also be defined at the domain level. They will be inherited by all tables of the domain.\nAny metadata property may be redefined at the attribute level."),(0,i.kt)("p",null,"Each field in the input file is defined using by its name, type and privacy level.\nWhen a header is present, fields do not need to be ordered, since Starlake uses the field name."),(0,i.kt)("p",null,"The attributes section in the YAML above should be read as follows :"),(0,i.kt)("table",null,(0,i.kt)("thead",{parentName:"table"},(0,i.kt)("tr",{parentName:"thead"},(0,i.kt)("th",{parentName:"tr",align:null},"Attribute"),(0,i.kt)("th",{parentName:"tr",align:null},"definition"))),(0,i.kt)("tbody",{parentName:"table"},(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:null},"name"),(0,i.kt)("td",{parentName:"tr",align:null},"Field name as specified in the header. If no header is present, this willthe field name in the ingested dataset.")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:null},"type"),(0,i.kt)("td",{parentName:"tr",align:null},"Type as defined in the Type Rules section above.")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:null},"required"),(0,i.kt)("td",{parentName:"tr",align:null},"Can this field be empty ?")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:null},"privacy"),(0,i.kt)("td",{parentName:"tr",align:null},"How should this field be altered during parsing. May be used to transform the output value.")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:null},"rename"),(0,i.kt)("td",{parentName:"tr",align:null},"When header is present in DSV files, this is the new field name in the ingested dataset")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:null},"metricType"),(0,i.kt)("td",{parentName:"tr",align:null},"When statistics generation is requested, should this field be treated as continuous, discrete or text value ? Valid values are CONTINUOUS, DISCRETE, TEXT, NONE")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:null},"array"),(0,i.kt)("td",{parentName:"tr",align:null},"true when this attribute is an array, false by default")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:null},"script"),(0,i.kt)("td",{parentName:"tr",align:null},"Allows you to add a new field computed from a UDF or a Spark SQL built-in standard function")))),(0,i.kt)("p",null,"Once the schema is validated, transformation at load time is done in the following order:"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"Scripted attributes are added and the script executed"),(0,i.kt)("li",{parentName:"ul"},"privacy function is applied to existing attributes"),(0,i.kt)("li",{parentName:"ul"},"attributes to ignore are removed"),(0,i.kt)("li",{parentName:"ul"},"attributes are renamed ")),(0,i.kt)("h3",{id:"privacy-strategy"},"Privacy Strategy"),(0,i.kt)("p",null,"Default valid values are NONE, HIDE, MD5, SHA1, SHA256, SHA512, AES(not implemented).\nCustom values may also be defined by adding a new privacy option in the application.conf. The default reference.conf file defines the following valid privacy\nstrategies:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-yaml"},'privacy:\n  options:\n    none: "ai.starlake.privacy.No",\n    hide: "ai.starlake.privacy.Hide",\n    hide10X: "ai.starlake.privacy.Hide(\\"X\\",10)",\n    approxLong20: "ai.starlake.privacy.ApproxLong(20)",\n    md5: "ai.starlake.privacy.Md5",\n    sha1: "ai.starlake.privacy.Sha1",\n    sha256: "ai.starlake.privacy.Sha256",\n    sha512: "ai.starlake.privacy.Sha512",\n    initials: "ai.starlake.privacy.Initials"\n  }\n}\n')),(0,i.kt)("p",null,"Any new privacy strategy should implement the following trait :"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"/**\n    * @param s: String  => Input string to encrypt\n    * @param colMap : Map[String, Option[String]] => Map of all the attributes and their corresponding values\n    * @param params: List[Any]  => Parameters passed to the algorithm as defined in the conf file.\n    *                               Parameter starting with '\"' is converted to a string\n    *                               Parameter containing a '.' is converted to a double\n    *                               Parameter equals to true of false is converted a boolean\n    *                               Anything else is converted to an int\n    * @return The encrypted string\n*/\n")),(0,i.kt)("p",null,"Below, the complete tables definition files."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="domain definition file load/sales/_config.comet.yml"',title:'"domain',definition:!0,file:!0,'load/sales/_config.comet.yml"':!0},'name: "hr"\ndirectory: "/mnt/incoming/sales"\nmetadata:\n  mode: "FILE"\n  format: "JSON"\n')),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="customers table definition file load/sales/customers.comet.yml"',title:'"customers',table:!0,definition:!0,file:!0,'load/sales/customers.comet.yml"':!0},'table:\n    name: "customers"\n    pattern: "customers-.*.psv"\n    metadata:\n      separator: "|"\n    attributes:\n      - name: "id"\n        type: "customerid"\n        required: true\n      - name: "signup"\n        type: "datetime"\n        required: false\n      - name: "contact"\n        type: "email"\n        required: false\n      - name: "birthdate"\n        type: "date"\n        required: false\n      - name: "name1"\n        type: "string"\n        required: false\n        rename: "firstname"\n      - name: "name2"\n        type: "string"\n        required: false\n        rename: "lastname"\n')),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="orders table definition file load/sales/orders.comet.yml"',title:'"orders',table:!0,definition:!0,file:!0,'load/sales/orders.comet.yml"':!0},'table:\n  name: "orders"\n  pattern: "orders-.*.csv"\n  merge:\n    key:\n      - "id"\n    delete: "customer_id is null"\n  metadata:\n    separator: ","\n  attributes:\n    - name: "order_id"\n      type: "string"\n      required: true\n      rename: "id"\n    - name: "customer_id"\n      type: "customerid"\n      required: false\n    - name: "amount"\n      type: "decimal"\n      required: false\n    - name: "seller_id"\n      type: "string"\n      required: false\n')),(0,i.kt)("admonition",{type:"note"},(0,i.kt)("p",{parentName:"admonition"},"The merge attribute above should be read as follows:")),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-yaml"},'merge:\n key:\n   - "id"\n delete: "customer_id is null"\n')),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},'When a new orders dataset is imported, only the last occurrence of the record identified by the key column "id" should be kept'),(0,i.kt)("li",{parentName:"ul"},"and any record imported with a null column_id should be removed from the existing dataset.")),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="HR domain definition file load/hr/_config.comet.yml"',title:'"HR',domain:!0,definition:!0,file:!0,'load/hr/_config.comet.yml"':!0},'name: "hr"\ndirectory: "/mnt/incoming/hr"\nmetadata:\n  mode: "FILE"\n  format: "JSON"\n')),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="sellers table definition file load/hr/sellers.comet.yml"',title:'"sellers',table:!0,definition:!0,file:!0,'load/hr/sellers.comet.yml"':!0},'table:\n  name: "sellers"\n  pattern: "sellers-.*.json"\n  metadata:\n    array: true\n    format: "SIMPLE_JSON"\n    write: "APPEND"\n  attributes:\n    - name: "id"\n      type: "string"\n      required: true\n    - name: "seller_email"\n      type: "email"\n      required: true\n    - name: "location_id"\n      type: "long"\n      required: true\n')),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="sellers table definition file load/hr/locations.comet.yml"',title:'"sellers',table:!0,definition:!0,file:!0,'load/hr/locations.comet.yml"':!0},'table:\n  name: "locations"\n  pattern: "locations-.*.json"\n  metadata:\n    format: "JSON"\n    write: "OVERWRITE"\n  attributes:\n    - name: "id"\n      type: "string"\n      required: true\n    - name: "address"\n      type: "struct"\n      required: true\n      attributes:\n        - name: "city"\n          type: "string"\n          required: true\n        - name: "stores"\n          type: "string"\n          array: true\n          required: false\n        - name: "country"\n          type: "string"\n          required: true\n')),(0,i.kt)("h2",{id:"write-strategy"},"Write Strategy"),(0,i.kt)("h3",{id:"partitioning"},"Partitioning"),(0,i.kt)("p",null,"You may control the partitioning strategy and tell Starlake how you wish to partition your\ndata on disk by specifying one or more attributes in the input file as partition columns."),(0,i.kt)("p",null,"If you want to use ingestion date/time as partition columns, you can use predefined attributes\n",(0,i.kt)("inlineCode",{parentName:"p"},"year"),", ",(0,i.kt)("inlineCode",{parentName:"p"},"month")," ",(0,i.kt)("inlineCode",{parentName:"p"},"day"),", ",(0,i.kt)("inlineCode",{parentName:"p"},"hour"),", ",(0,i.kt)("inlineCode",{parentName:"p"},"minute")," prefixed by ",(0,i.kt)("inlineCode",{parentName:"p"},"comet_"),". These columns will\nappear as regular attributes in the resulting dataset and without the prefix ",(0,i.kt)("inlineCode",{parentName:"p"},"comet_")),(0,i.kt)("p",null,"Below an example of how to partition by ingestion year, month and day."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-yaml"},'metadata:\n  partition:\n    attributes:\n      - "comet_year"\n      - "comet_month"\n      - "comet_day"\n')),(0,i.kt)("h3",{id:"compaction-strategy"},"Compaction Strategy"),(0,i.kt)("p",null,"When saving files as parquet or orc or whatever, the optimal number of partitions depend on the dataset size,\nnumber of records, the size of each record and the block size."),(0,i.kt)("p",null,"The goal is to optimise the number of partitions during the write phase."),(0,i.kt)("p",null,"You have 3 choices available :"),(0,i.kt)("h4",{id:"solution-1--naive-compaction"},"Solution 1 : Naive Compaction"),(0,i.kt)("ol",null,(0,i.kt)("li",{parentName:"ol"},"Save the file in a temporary location"),(0,i.kt)("li",{parentName:"ol"},"Get the dataset size."),(0,i.kt)("li",{parentName:"ol"},"Divide the dataset size by the  block size to get the number of partitions"),(0,i.kt)("li",{parentName:"ol"},"Save the dataset to the target location with the computed number of partitions")),(0,i.kt)("p",null,"The main drawback of this approach is that we need to save the file twice."),(0,i.kt)("h4",{id:"solution-2--sampling"},"Solution 2 : Sampling"),(0,i.kt)("ol",null,(0,i.kt)("li",{parentName:"ol"},"Get a percentage of the records in the dataframe before saving it."),(0,i.kt)("li",{parentName:"ol"},"Save it to a temporary location"),(0,i.kt)("li",{parentName:"ol"},"Estimate the size of the final dataset on HDFS based on the size of the sample"),(0,i.kt)("li",{parentName:"ol"},"Compute the number of partitions based on this estimation"),(0,i.kt)("li",{parentName:"ol"},"Save the dataset to the target location with the computed number of partitions")),(0,i.kt)("p",null,"The Naive solution is in fact identical to the Sampling one with a sampling percentage of 100%."),(0,i.kt)("h4",{id:"solution-3--absolute-compaction"},"Solution 3 : Absolute Compaction"),(0,i.kt)("p",null,"The number of partitions is defined by the user at the schema level."),(0,i.kt)("p",null,"Example :"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"0.0 => Means no optimisation.")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"1.0 => Na\xefve Compaction")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Any integer between 1 ... Int.max => Absolute number of partitions"))),(0,i.kt)("p",null,"Below an example of compaction based on a sampling of 20%"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-yaml"},'metadata:\n  partition:\n    sampling: 0.2 # compute number of partitions based on the size on disk of a sampling of 20% of the dataset\n    attributes:\n      - "comet_year"\n      - "comet_month"\n      - "comet_day"\n')),(0,i.kt)("p",null,"With the types catalog, file schemas and save strategy defined we are ready to ingest"),(0,i.kt)("h2",{id:"load-workflow"},"Load Workflow"),(0,i.kt)("p",null,"The ingestion process follows the steps below :"),(0,i.kt)("ol",null,(0,i.kt)("li",{parentName:"ol"},"Import Step : Files are imported to the cluster in the pending area."),(0,i.kt)("li",{parentName:"ol"},"Load Step : Files in the pending area are submitted for ingestion to the Job Manager (Airflow for example)."),(0,i.kt)("li",{parentName:"ol"},"Ingestion Step: Files are validated and converted to a cluster defined file format (parquet, orc ...) and saved as Hive tables.")),(0,i.kt)("p",null,"Before running the steps below, please configure first the environment variables\nas described in the Configuration section."),(0,i.kt)("h2",{id:"import-step"},"Import Step"),(0,i.kt)("h3",{id:"how-it-works"},"How it works"),(0,i.kt)("ol",null,(0,i.kt)("li",{parentName:"ol"},"On startup, all the domain definitions files are loaded from the folder /tmp/metadata/domains"),(0,i.kt)("li",{parentName:"ol"},"Directories referenced by the ",(0,i.kt)("inlineCode",{parentName:"li"},"directory")," attribute in the YAML domain definition files are scanned for incoming files. The incoming folder must be accessible locally or through a mount point."),(0,i.kt)("li",{parentName:"ol"},"Any compressed file or file with any default extension or with one of the extension defined by the ",(0,i.kt)("inlineCode",{parentName:"li"},"extensions")," attribute are moved to the cluster in the domain pending folder, /tmp/datasets/pending/",(0,i.kt)("inlineCode",{parentName:"li"},"DOMAIN NAME"),"/ by default.")),(0,i.kt)("h3",{id:"running-it"},"Running it"),(0,i.kt)("p",null,"To run the import step, you have to have the spark & hadoop\nclient libraries in your classpath. You may get them automatically\nby running the import step:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-shell"},"$ starlake.sh import\n")),(0,i.kt)("h2",{id:"load-step"},"Load Step"),(0,i.kt)("h3",{id:"how-it-works-1"},"How it works"),(0,i.kt)("p",null,"The Load process will scan the all domain pending folders in the cluster.\nAny file that matches the pattern defined by the ",(0,i.kt)("inlineCode",{parentName:"p"},"pattern")," attribute in\nthe schema section of the domain definition file is submitted to the Job Manager.\nFiles that do not match any pattern are moved to the unresolved\nfolder, /tmp/datasets/unresolved/",(0,i.kt)("inlineCode",{parentName:"p"},"DOMAIN NAME"),"/ by default."),(0,i.kt)("p",null,"Once copied to the pending folder, a request for ingestion (see step below) is submitted to the Job Manager."),(0,i.kt)("admonition",{type:"note"},(0,i.kt)("p",{parentName:"admonition"},"By default the ",(0,i.kt)("inlineCode",{parentName:"p"},"simple")," job manager is invoked. This simple manager\nused for debugging & testing purpose would launch the ingestion step inside the current process.\nIn production, you would configure a job manager running on your cluster.\nStarlake comes with the ",(0,i.kt)("inlineCode",{parentName:"p"},"airflow")," job manager and sample DAGs required to run all three steps.")),(0,i.kt)("h3",{id:"running-it-1"},"Running it"),(0,i.kt)("p",null,"To run the import step, you have to have the spark & hadoop\nclient libraries in your classpath. You may get them automatically\nby running the load step:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-shell"},"$ starlake.sh load\n")),(0,i.kt)("h2",{id:"ingestion-step"},"Ingestion Step"),(0,i.kt)("h3",{id:"how-it-works-2"},"How it works"),(0,i.kt)("p",null,"Unlike the steps above, this step does not scan any folder.\nIt takes as its parameters the domain name, schema name and\nfull path of the file that need to be ingested. That's why it is usually\ninvoked through request submitted to a job manager by at the Load Step."),(0,i.kt)("h3",{id:"running-it-2"},"Running it"),(0,i.kt)("p",null,"To interactively run it, copy the input file in the pending area\nof a domain and run it as follows:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-shell"},"$ starlake.sh ingest DOMAIN_NAME SCHEMA_NAME hdfs://datasets/domain/pending/file.dsv\n")),(0,i.kt)("h2",{id:"export-step"},"Export Step"),(0,i.kt)("h3",{id:"how-it-works-3"},"How it works"),(0,i.kt)("p",null,"This step is concerned with exporting the dataset to Elasticsearch / SQL Database / CSV or JSON file\nIt takes as its parameters the domain name, schema name and\nfull path of the file that need to be ingested. That's why it is usually\ninvoked through request submitted to a job manager by at the Load Step."),(0,i.kt)("h3",{id:"running-it-3"},"Running it"),(0,i.kt)("p",null,"To interactively run it, copy the input file in the pending area\nof a domain and run it as follows:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-shell"},"$ starlake.sh ingest DOMAIN_NAME SCHEMA_NAME hdfs://datasets/domain/pending/file.dsv\n")))}u.isMDXComponent=!0}}]);