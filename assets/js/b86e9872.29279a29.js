"use strict";(self.webpackChunkstarlake=self.webpackChunkstarlake||[]).push([[1511],{3905:(e,t,a)=>{a.d(t,{Zo:()=>m,kt:()=>h});var n=a(67294);function r(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function s(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function i(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?s(Object(a),!0).forEach((function(t){r(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):s(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function o(e,t){if(null==e)return{};var a,n,r=function(e,t){if(null==e)return{};var a,n,r={},s=Object.keys(e);for(n=0;n<s.length;n++)a=s[n],t.indexOf(a)>=0||(r[a]=e[a]);return r}(e,t);if(Object.getOwnPropertySymbols){var s=Object.getOwnPropertySymbols(e);for(n=0;n<s.length;n++)a=s[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(r[a]=e[a])}return r}var l=n.createContext({}),d=function(e){var t=n.useContext(l),a=t;return e&&(a="function"==typeof e?e(t):i(i({},t),e)),a},m=function(e){var t=d(e.components);return n.createElement(l.Provider,{value:t},e.children)},u="mdxType",p={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},c=n.forwardRef((function(e,t){var a=e.components,r=e.mdxType,s=e.originalType,l=e.parentName,m=o(e,["components","mdxType","originalType","parentName"]),u=d(a),c=r,h=u["".concat(l,".").concat(c)]||u[c]||p[c]||s;return a?n.createElement(h,i(i({ref:t},m),{},{components:a})):n.createElement(h,i({ref:t},m))}));function h(e,t){var a=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var s=a.length,i=new Array(s);i[0]=c;var o={};for(var l in t)hasOwnProperty.call(t,l)&&(o[l]=t[l]);o.originalType=e,o[u]="string"==typeof e?e:r,i[1]=o;for(var d=2;d<s;d++)i[d]=a[d];return n.createElement.apply(null,i)}return n.createElement.apply(null,a)}c.displayName="MDXCreateElement"},88209:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>l,contentTitle:()=>i,default:()=>p,frontMatter:()=>s,metadata:()=>o,toc:()=>d});var n=a(87462),r=(a(67294),a(3905));const s={sidebar_position:40,title:"Examples"},i="Examples",o={unversionedId:"reference/examples",id:"version-0.8.0/reference/examples",title:"Examples",description:"Extract",source:"@site/versioned_docs/version-0.8.0/reference/40.examples.md",sourceDirName:"reference",slug:"/reference/examples",permalink:"/starlake/docs/0.8.0/reference/examples",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/versioned_docs/version-0.8.0/reference/40.examples.md",tags:[],version:"0.8.0",sidebarPosition:40,frontMatter:{sidebar_position:40,title:"Examples"},sidebar:"starlakeSidebar",previous:{title:"Transform",permalink:"/starlake/docs/0.8.0/reference/transform"},next:{title:"Scheduling",permalink:"/starlake/docs/0.8.0/reference/scheduling"}},l={},d=[{value:"Extract",id:"extract",level:2},{value:"Extract All Tables:",id:"extract-all-tables",level:3},{value:"Extract Specific Tables:",id:"extract-specific-tables",level:3},{value:"Load",id:"load",level:2},{value:"Load to Parquet",id:"load-to-parquet",level:3},{value:"Load to BigQuery",id:"load-to-bigquery",level:3},{value:"Load to SQL Database",id:"load-to-sql-database",level:3},{value:"Load to Elasticsearch",id:"load-to-elasticsearch",level:3},{value:"Custom ES Template",id:"custom-es-template",level:4},{value:"Transform",id:"transform",level:2},{value:"Parquet to Parquet",id:"parquet-to-parquet",level:3},{value:"Transform Parquet to DSV",id:"transform-parquet-to-dsv",level:3},{value:"Transform Parquet to BigQuery",id:"transform-parquet-to-bigquery",level:3},{value:"BigQuery to BigQuery",id:"bigquery-to-bigquery",level:3},{value:"BigQuery to CSV",id:"bigquery-to-csv",level:3},{value:"BigQuery to Parquet",id:"bigquery-to-parquet",level:3},{value:"Parquet to Elasticsearch",id:"parquet-to-elasticsearch",level:3},{value:"BigQuery to Elasticsearch",id:"bigquery-to-elasticsearch",level:3},{value:"BigQuery to SQL Database",id:"bigquery-to-sql-database",level:3},{value:"Parquet to SQL Database",id:"parquet-to-sql-database",level:3},{value:"SQL Database to SQL Database",id:"sql-database-to-sql-database",level:3}],m={toc:d},u="wrapper";function p(e){let{components:t,...a}=e;return(0,r.kt)(u,(0,n.Z)({},m,a,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"examples"},"Examples"),(0,r.kt)("h2",{id:"extract"},"Extract"),(0,r.kt)("p",null,"This sample is available in the ",(0,r.kt)("inlineCode",{parentName:"p"},"samples/extract")," directory\nFirst you need to set the JDBC connection to the database. Below an example on a H2 database:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-hocon"},'connections {\n  h2-sample-db {\n    format = "jdbc"\n    options {\n      url: "jdbc:h2:file:/my/h2db/path",\n      driver: "org.h2.Driver"\n    }\n  }\n}\n')),(0,r.kt)("p",null,"Next create a file describing the schema you want to import. We provide below 2 examples."),(0,r.kt)("h3",{id:"extract-all-tables"},"Extract All Tables:"),(0,r.kt)("p",null,"Extract all objects from the ",(0,r.kt)("inlineCode",{parentName:"p"},"PUBLIC")," schema."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'jdbcSchemas:\n  - connection: "h2-sample-db" # Connection name as defined in the connections section of the application.conf file\n    schema: "PUBLIC" # Database schema where tables are located\n    tables:\n      - name: "*" # Takes all tables\n    tableTypes: # One or many of the types below\n      - "TABLE"\n      - "VIEW"\n      - "SYSTEM TABLE"\n      - "GLOBAL TEMPORARY"\n      - "LOCAL TEMPORARY"\n      - "ALIAS"\n      - "SYNONYM"\n    template: "domain-template.yml" # Metadata to use for the generated YML file.\n\n')),(0,r.kt)("h3",{id:"extract-specific-tables"},"Extract Specific Tables:"),(0,r.kt)("p",null,"Extract only the selected tables from the ",(0,r.kt)("inlineCode",{parentName:"p"},"PUBLIC"),"schema and only the selected columns from the ",(0,r.kt)("inlineCode",{parentName:"p"},"votes")," table."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'jdbcSchemas:\n  - connection: "h2-sample-db" # Connection name as defined in the connections section of the application.conf file\n    schema: "PUBLIC" # Database schema where tables are located\n    tables:\n      - name: "speakers"\n      - name: "votes"\n        columns:\n          - speaker_id\n          - id\n          - rating\n    tableTypes: # One or many of the types below\n      - "TABLE"\n    templateFile: "domain-template.yml" # Metadata to use for the generated YML file.\n')),(0,r.kt)("p",null,"The ",(0,r.kt)("inlineCode",{parentName:"p"},"templateFile")," section are used to set the default values for the metadata fields in the generated ",(0,r.kt)("inlineCode",{parentName:"p"},"load")," file. "),(0,r.kt)("h2",{id:"load"},"Load"),(0,r.kt)("p",null,"This section describes how to import text files (eq. json / CSV) files into your Data Factory."),(0,r.kt)("h3",{id:"load-to-parquet"},"Load to Parquet"),(0,r.kt)("p",null,"Files will be ingested and stored in parquet format in the ",(0,r.kt)("inlineCode",{parentName:"p"},"$SL_DATASETS/sales/customers")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"$SL_DATASETS/sales/orders")," files."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'load:\n    name: "sales"\n    directory: "__SL_TEST_ROOT__/incoming/sales"\n    metadata:\n      mode: "FILE"\n      format: "DSV"\n      withHeader: true\n      quote: "\\""\n      escape: "\\\\"\n      write: "APPEND"\n    schemas:\n      - name: "customers"\n        pattern: "customers-.*.psv"\n        metadata:\n          separator: "|"\n        attributes:\n          - name: "id"\n            type: "customerid"\n            required: true\n          - name: "signup"\n            type: "datetime"\n            required: false\n          - name: "contact"\n            type: "email"\n            required: false\n          - name: "birthdate"\n            type: "date"\n            required: false\n          - name: "name1"\n            type: "string"\n            required: false\n            rename: "firstname"\n          - name: "name2"\n            type: "string"\n            required: false\n            rename: "lastname"\n      - name: "orders"\n        pattern: "orders-.*.csv"\n        merge:\n          key:\n            - "id"\n          delete: "customer_id is null"\n        metadata:\n          separator: ","\n        attributes:\n          - name: "order_id"\n            type: "string"\n            required: true\n            rename: "id"\n          - name: "customer_id"\n            type: "customerid"\n            required: false\n          - name: "amount"\n            type: "decimal"\n            required: false\n          - name: "seller_id"\n            type: "string"\n            required: false\n')),(0,r.kt)("h3",{id:"load-to-bigquery"},"Load to BigQuery"),(0,r.kt)("p",null,"Based on the ",(0,r.kt)("a",{parentName:"p",href:"#load-to-parquet"},"Load to parquet")," example, the only thing we add is the /metadata/sink section\nFiles will be stored in the ",(0,r.kt)("inlineCode",{parentName:"p"},"customers")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"orders")," BigQuery tables under the ",(0,r.kt)("inlineCode",{parentName:"p"},"sales")," BigQuery dataset"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'load:\n    name: "sales"\n    directory: "/incoming/sales"\n    metadata:\n      mode: "FILE"\n      format: "DSV"\n      withHeader: true\n      quote: "\\""\n      escape: "\\\\"\n      write: "APPEND"\n      sink:\n        type: BQ\n    schemas:\n      - name: "customers"\n        pattern: "customers-.*.psv"\n        metadata:\n          separator: "|"\n        attributes:\n          - name: "id"\n            type: "customerid"\n            required: true\n          - name: "signup"\n            type: "datetime"\n            required: false\n          - name: "contact"\n            type: "email"\n            required: false\n          - name: "birthdate"\n            type: "date"\n            required: false\n          - name: "name1"\n            type: "string"\n            required: false\n            rename: "firstname"\n          - name: "name2"\n            type: "string"\n            required: false\n            rename: "lastname"\n      - name: "orders"\n        pattern: "orders-.*.csv"\n        merge:\n          key:\n            - "id"\n          delete: "customer_id is null"\n        metadata:\n          separator: ","\n        attributes:\n          - name: "order_id"\n            type: "string"\n            required: true\n            rename: "id"\n          - name: "customer_id"\n            type: "customerid"\n            required: false\n          - name: "amount"\n            type: "decimal"\n            required: false\n          - name: "seller_id"\n            type: "string"\n            required: false\n')),(0,r.kt)("h3",{id:"load-to-sql-database"},"Load to SQL Database"),(0,r.kt)("p",null,"Based on the ",(0,r.kt)("a",{parentName:"p",href:"#load-to-parquet"},"Load to parquet")," example, we need to"),(0,r.kt)("ol",null,(0,r.kt)("li",{parentName:"ol"},"Add  the /metadata/sink section")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'load:\n    name: "hr"\n    directory: "/incoming/hr"\n    metadata:\n      mode: "FILE"\n      format: "JSON"\n      sink:\n        type: JDBC\n        connection: my_connection\n        partitions: 10\n        batchSize: 1000\n    schemas:\n      - name: "sellers"\n        pattern: "sellers-.*.json"\n        metadata:\n          array: true\n          format: "SIMPLE_JSON"\n          write: "APPEND"\n        attributes:\n          - name: "id"\n            type: "string"\n            required: true\n          - name: "seller_email"\n            type: "email"\n            required: true\n          - name: "location_id"\n            type: "long"\n            required: true\n      - name: "locations"\n        pattern: "locations-.*.json"\n        metadata:\n          format: "JSON"\n          write: "OVERWRITE"\n        attributes:\n          - name: "id"\n            type: "string"\n            required: true\n          - name: "address"\n            type: "struct"\n            required: true\n            attributes:\n              - name: "city"\n                type: "string"\n                required: true\n                metricType: "discrete"\n              - name: "stores"\n                type: "string"\n                array: true\n                required: false\n              - name: "country"\n                type: "string"\n                required: true\n                metricType: "discrete"\n')),(0,r.kt)("ol",{start:2},(0,r.kt)("li",{parentName:"ol"},"Add to the jdbc section a connection with the name specified in the /medata/sink/connection property")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-javascript"},'jdbc = {\n  "my_connection": {\n    uri = "jdbc:postgresql://127.0.0.1:5403/mydb?user=postgres&password=XXXX-XXXX-XXXX",\n    user = "postgres",\n    password = "XXXX-XXXX-XXXX",\n    driver = "org.postgresql.Driver"\n  }\n}\n')),(0,r.kt)("h3",{id:"load-to-elasticsearch"},"Load to Elasticsearch"),(0,r.kt)("p",null,"Based on the example ",(0,r.kt)("a",{parentName:"p",href:"#load-to-parquet"},"Load to parquet")," example, we add is the /metadata/sink section to both schemas."),(0,r.kt)("p",null,"For the sake of the example, we added a field to the location schema to highlight how timestamped indexes may be handled.\nIndexes will be named after the domain and schema names suffixed by the timestamp if present."),(0,r.kt)("p",null,"The ",(0,r.kt)("inlineCode",{parentName:"p"},"orders")," index will be named ",(0,r.kt)("inlineCode",{parentName:"p"},"sales_orders")," and the ",(0,r.kt)("inlineCode",{parentName:"p"},"customers")," index will have a name similar to ",(0,r.kt)("inlineCode",{parentName:"p"},"sales_customers-2020.01.31")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'load:\n    name: "sales"\n    directory: "__SL_TEST_ROOT__/incoming/sales"\n    metadata:\n      mode: "FILE"\n      format: "DSV"\n      withHeader: true\n      quote: "\\""\n      escape: "\\\\"\n      write: "APPEND"\n      sink:\n        type: BQ\n    schemas:\n      - name: "customers"\n        pattern: "customers-.*.psv"\n        metadata:\n          separator: "|"\n          sink:\n            type: ES\n            timestamp: "{signup|yyyy.MM.dd}"\n        attributes:\n          - name: "id"\n            type: "customerid"\n            required: true\n          - name: "signup"\n            type: "datetime"\n            required: false\n          - name: "contact"\n            type: "email"\n            required: false\n          - name: "birthdate"\n            type: "date"\n            required: false\n          - name: "name1"\n            type: "string"\n            required: false\n            rename: "firstname"\n          - name: "name2"\n            type: "string"\n            required: false\n            rename: "lastname"\n      - name: "orders"\n        pattern: "orders-.*.csv"\n        merge:\n          key:\n            - "id"\n          delete: "customer_id is null"\n        metadata:\n          separator: ","\n          sink:\n            type: ES\n        attributes:\n          - name: "order_id"\n            type: "string"\n            required: true\n            rename: "id"\n          - name: "customer_id"\n            type: "customerid"\n            required: false\n          - name: "amount"\n            type: "decimal"\n            required: false\n          - name: "seller_id"\n            type: "string"\n            required: false\n')),(0,r.kt)("h4",{id:"custom-es-template"},"Custom ES Template"),(0,r.kt)("p",null,"By default, Starlake will infer from the dataset schema the properties and their types and create the ES template accordingly.\nThe default template template is shown below. The variable  ",(0,r.kt)("inlineCode",{parentName:"p"},"__ATTRIBUTES__")," is substituted by the Starlake with\nthe ES representation of the attributes."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-json"},'{\n  "index_patterns": ["{{domain}}.{{schema}}", "{{domain}}.{{schema}}-*"],\n  "settings": {\n    "number_of_shards": "1",\n    "number_of_replicas": "0"\n  },\n  "mappings": {\n    "_doc": {\n      "_source": {\n        "enabled": true\n      },\n      "properties": {\n        {{attributes}}\n      }\n    }\n  }\n}\n')),(0,r.kt)("p",null,"You may customize your ES template by creating a similar file with your own custom properties for a specific schema by putting it\nin the file with the following name ",(0,r.kt)("inlineCode",{parentName:"p"},"$SL_ROOT/metadata/mapping/${domainName}/${schemaName}.json"),"."),(0,r.kt)("p",null,"You may inject the domain and schema names using the ",(0,r.kt)("inlineCode",{parentName:"p"},"{{domain}}")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"{{schema}}")," substitution variables."),(0,r.kt)("h2",{id:"transform"},"Transform"),(0,r.kt)("h3",{id:"parquet-to-parquet"},"Parquet to Parquet"),(0,r.kt)("p",null,"Will load the dataset ",(0,r.kt)("inlineCode",{parentName:"p"},"accepted/graduateProgram")," under ",(0,r.kt)("inlineCode",{parentName:"p"},"$SL_DATASETS")," directory from the configured storage.\nAn absolute path may also be specified."),(0,r.kt)("p",null,"This example create two views : One temporary view in the ",(0,r.kt)("inlineCode",{parentName:"p"},"views")," section, and another one in the ",(0,r.kt)("inlineCode",{parentName:"p"},"presql")," section.\nNote that the sql request in the ",(0,r.kt)("inlineCode",{parentName:"p"},"presql")," section uses the view defined in the ",(0,r.kt)("inlineCode",{parentName:"p"},"views")," section."),(0,r.kt)("p",null,"The resulting file will be stored in the ",(0,r.kt)("inlineCode",{parentName:"p"},"$SL_DATASETS/business/graduateProgram/output")," directory."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'---\ntransform:\n    name: "graduateProgram"\n    views:\n      graduate_View: "fs:accepted/graduateProgram"\n    tasks:\n      - domain: "graduateProgram"\n        area: "business"\n        dataset: "output"\n        write: "OVERWRITE"\n        presql: |\n          create or replace view graduate_agg_view\n          select degree,\n            department,\n            school\n          from graduate_View\n          where school={{school}}\n    \n        sql: SELECT * FROM graduate_agg_view\n')),(0,r.kt)("h3",{id:"transform-parquet-to-dsv"},"Transform Parquet to DSV"),(0,r.kt)("p",null,"Based ont the ",(0,r.kt)("a",{parentName:"p",href:"#parquet-to-parquet"},"parquet to parquet")," example, we add the format property to request a csv output\nand set coalesce to ",(0,r.kt)("inlineCode",{parentName:"p"},"true")," to output everything in a single CSV file."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'---\ntransform:\n    name: "graduateProgram"\n    format: "csv"\n    coalesce: true\n    views:\n      graduate_View: "fs:accepted/graduateProgram"\n    tasks:\n      - domain: "graduateProgram"\n        area: "business"\n        dataset: "output"\n        write: "OVERWRITE"\n        presql: |\n          create or replace view graduate_agg_view\n          select degree,\n            department,\n            school\n          from graduate_View\n          where school={{school}}\n    \n        sql: SELECT * FROM graduate_agg_view\n')),(0,r.kt)("h3",{id:"transform-parquet-to-bigquery"},"Transform Parquet to BigQuery"),(0,r.kt)("p",null,"Based ont the ",(0,r.kt)("a",{parentName:"p",href:"#parquet-to-parquet"},"parquet to parquet")," example, we add the sink section to force the task to store the SQL result in BigQuery"),(0,r.kt)("p",null,"The result will store in the current project under the ",(0,r.kt)("inlineCode",{parentName:"p"},"business")," BigQuery dataset in the ",(0,r.kt)("inlineCode",{parentName:"p"},"output")," table."),(0,r.kt)("p",null,"You may also specify the target project in the ",(0,r.kt)("inlineCode",{parentName:"p"},"/tasks/dataset")," property using the syntax ",(0,r.kt)("inlineCode",{parentName:"p"},"PROJECT_ID:business")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'---\ntransform:\n    name: "graduateProgram"\n    views:\n      graduate_View: "fs:accepted/graduateProgram"\n    tasks:\n      - domain: "graduateProgram"\n        area: "business"\n        dataset: "output"\n        write: "OVERWRITE"\n        sink:\n            type: BQ\n            location: EU\n        presql: |\n          create or replace view graduate_agg_view\n          select degree,\n            department,\n            school\n          from graduate_View\n          where school={{school}}\n    \n        sql: SELECT * FROM graduate_agg_view\n')),(0,r.kt)("h3",{id:"bigquery-to-bigquery"},"BigQuery to BigQuery"),(0,r.kt)("p",null,"We may use the Spark (SPARK) or BigQuery (BQ) engine. When using the BQ engine, no spark cluster is needed."),(0,r.kt)("p",null,"You may want to use the Spark engine if you need to run your jobs to stay agnostic to the underlying storage or\nif you need your jobs to overwrite only the partitions present in the resulting SQL."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'---\ntransform:\n    name: "graduateProgram"\n    views:\n      graduate_View: "bq:gcp_project_id:bqdataset/graduateProgram"\n    tasks:\n      - domain: "graduateProgram"\n        sink:\n            type: BQ\n        area: "business"\n        dataset: "output"\n        write: "OVERWRITE"\n        sql: SELECT * FROM graduate_View\n')),(0,r.kt)("h3",{id:"bigquery-to-csv"},"BigQuery to CSV"),(0,r.kt)("h3",{id:"bigquery-to-parquet"},"BigQuery to Parquet"),(0,r.kt)("h3",{id:"parquet-to-elasticsearch"},"Parquet to Elasticsearch"),(0,r.kt)("h3",{id:"bigquery-to-elasticsearch"},"BigQuery to Elasticsearch"),(0,r.kt)("h3",{id:"bigquery-to-sql-database"},"BigQuery to SQL Database"),(0,r.kt)("h3",{id:"parquet-to-sql-database"},"Parquet to SQL Database"),(0,r.kt)("h3",{id:"sql-database-to-sql-database"},"SQL Database to SQL Database"))}p.isMDXComponent=!0}}]);