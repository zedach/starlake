"use strict";(self.webpackChunkstarlake=self.webpackChunkstarlake||[]).push([[8238],{3905:(e,t,a)=>{a.d(t,{Zo:()=>p,kt:()=>b});var n=a(67294);function r(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function o(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function i(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?o(Object(a),!0).forEach((function(t){r(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):o(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function l(e,t){if(null==e)return{};var a,n,r=function(e,t){if(null==e)return{};var a,n,r={},o=Object.keys(e);for(n=0;n<o.length;n++)a=o[n],t.indexOf(a)>=0||(r[a]=e[a]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(n=0;n<o.length;n++)a=o[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(r[a]=e[a])}return r}var s=n.createContext({}),c=function(e){var t=n.useContext(s),a=t;return e&&(a="function"==typeof e?e(t):i(i({},t),e)),a},p=function(e){var t=c(e.components);return n.createElement(s.Provider,{value:t},e.children)},d="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},m=n.forwardRef((function(e,t){var a=e.components,r=e.mdxType,o=e.originalType,s=e.parentName,p=l(e,["components","mdxType","originalType","parentName"]),d=c(a),m=r,b=d["".concat(s,".").concat(m)]||d[m]||u[m]||o;return a?n.createElement(b,i(i({ref:t},p),{},{components:a})):n.createElement(b,i({ref:t},p))}));function b(e,t){var a=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=a.length,i=new Array(o);i[0]=m;var l={};for(var s in t)hasOwnProperty.call(t,s)&&(l[s]=t[s]);l.originalType=e,l[d]="string"==typeof e?e:r,i[1]=l;for(var c=2;c<o;c++)i[c]=a[c];return n.createElement.apply(null,i)}return n.createElement.apply(null,a)}m.displayName="MDXCreateElement"},28553:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>s,contentTitle:()=>i,default:()=>u,frontMatter:()=>o,metadata:()=>l,toc:()=>c});var n=a(87462),r=(a(67294),a(3905));const o={sidebar_position:10},i="Extract",l={unversionedId:"reference/extract",id:"version-0.8.0/reference/extract",title:"Extract",description:"This step is optional and useful only if you intend to extract data from a SQL Database into",source:"@site/versioned_docs/version-0.8.0/reference/15.extract.md",sourceDirName:"reference",slug:"/reference/extract",permalink:"/starlake/docs/0.8.0/reference/extract",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/versioned_docs/version-0.8.0/reference/15.extract.md",tags:[],version:"0.8.0",sidebarPosition:10,frontMatter:{sidebar_position:10},sidebar:"starlakeSidebar",previous:{title:"Environment",permalink:"/starlake/docs/0.8.0/reference/environment"},next:{title:"Load",permalink:"/starlake/docs/0.8.0/reference/load"}},s={},c=[{value:"Configuration",id:"configuration",level:2},{value:"Extract table schemas",id:"extract-table-schemas",level:3},{value:"Extract Data",id:"extract-data",level:2}],p={toc:c},d="wrapper";function u(e){let{components:t,...o}=e;return(0,r.kt)(d,(0,n.Z)({},p,o,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"extract"},"Extract"),(0,r.kt)("p",null,"This step is optional and useful only if you intend to extract data from a SQL Database into\na set of files before ingesting it into a warehouse."),(0,r.kt)("p",null,(0,r.kt)("img",{src:a(16243).Z,width:"1200",height:"680"})),(0,r.kt)("h2",{id:"configuration"},"Configuration"),(0,r.kt)("p",null,"We'll have to go through the following steps to extract data from a JDBC compliant database\ninto a set of files that will be loaded into our datawarehouse:"),(0,r.kt)("ol",null,(0,r.kt)("li",{parentName:"ol"},"Extract the database table schemas"),(0,r.kt)("li",{parentName:"ol"},"Extract the data")),(0,r.kt)("p",null,(0,r.kt)("img",{src:a(86259).Z,width:"1860",height:"1474"})),(0,r.kt)("h3",{id:"extract-table-schemas"},"Extract table schemas"),(0,r.kt)("p",null,"To extract the tables into a set of DSV files, create the dbextract.sl.yml YAML specification file\nthat describe the tables and columns you are willing to extract using the following syntax:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'extract:\n    connectionRef: "mypostgresdb" # Connection name as defined in the connections section of the application.conf file\n    jdbcSchemas:\n        - catalog: "business" # Optional catalog name in the target database\n          schema: "public" # Database schema where tables are located\n          tables:\n            - name: "user"\n              write: "Append" # Append or Overwrite to describe how the data should be loaded\n              pattern: "user-.*.csv" # File pattern to recognize when loading this type of table\n              fechSize: 100 # May be defined and inherited from top or schema level\n              partitionColumn: "id" # Only values more recent (higher) than the last exported ones will be exported. \n              numPartitions: 2 # level of parallelism. May be inherited from the schema level\n              columns: # optional list of columns, if not present all columns should be exported.\n                - id\n                - email\n            - name: product # All columns should be exported\n            - name: "*" # Ignore any other table spec. Just export all tables\n          tableTypes: # One or many of the types below\n            - "TABLE"\n            - "VIEW"\n            - "SYSTEM TABLE"\n            - "GLOBAL TEMPORARY"\n            - "LOCAL TEMPORARY"\n            - "ALIAS"\n            - "SYNONYM"\n          template: "/absolute/path/domain-template.yml" # Metadata to use for the generated YML file.\n')),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="metadata/application.sl.yml"',title:'"metadata/application.sl.yml"'},'connections:\n  mypostgresdb:\n    type: "jdbc"\n    options:\n      url: "jdbc:postgresql://127.0.0.1:5403/comet",\n      user: "postgres",\n      password: "ficpug-Podbid-7fobnu",\n      driver: "org.postgresql.Driver"\n\n  audit: # Connection to the database that will register the extract history \n    type: "jdbc"\n    options:\n      url: "jdbc:postgresql://127.0.0.1:5403/comet",\n      user: "postgres",\n      password: "ficpug-Podbid-7fobnu",\n      driver: "org.postgresql.Driver"\n')),(0,r.kt)("admonition",{type:"note"},(0,r.kt)("p",{parentName:"admonition"},"we may replace connectionRef tag by the connection tag and provide the connection options inline right inside the YAML configuration file as follows:")),(0,r.kt)("admonition",{type:"note"},(0,r.kt)("p",{parentName:"admonition"},"if you need to set common jdbc schema attributes, you can use globalJdbcSchema on the same level as jdbcSchemas and define the same attributes. Tables can't be set commonly."),(0,r.kt)("pre",{parentName:"admonition"},(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'extract:\n    connection:\n      url: "jdbc:postgresql://127.0.0.1:5403/comet",\n      user: "postgres",\n      password: "ficpug-Podbid-7fobnu"\n      driver: "org.postgresql.Driver"\n    jdbcSchemas:\n        - catalog: "business" # Optional catalog name in the target database\n          schema: "public" # Database schema where tables are located\n          tables:\n      ...\n\n'))),(0,r.kt)("p",null,"To extract all the tables, simply set the ",(0,r.kt)("inlineCode",{parentName:"p"},"name"),"attribute below the ",(0,r.kt)("inlineCode",{parentName:"p"},"table"),' attribute to "*"'),(0,r.kt)("p",null,"To import all the columns of a table, do not define the columns attribute."),(0,r.kt)("p",null,"This will generate a YAML file with a metadata section in the metadata/load directory:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-bash"},"$ starlake.sh extract-schema --config dbextract.sl.yml # extract description\n")),(0,r.kt)("h2",{id:"extract-data"},"Extract Data"),(0,r.kt)("p",null,"Simply run the "),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-bash"},"$ starlake.sh extract-data \\\n    --config dbextract.sl.yml \\ # extract description\n    --output-dir data-out \\ # output directory\n    --separator ';' \\ # use ';' as a separator\n    --clean # clean output folder first \n")))}u.isMDXComponent=!0},86259:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/extract-detailed-19d789144ca3d0b683f1136dbb3b414d.png"},16243:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/extract-general-e23869d2c6741cdfc751e18cb8d80b73.png"}}]);