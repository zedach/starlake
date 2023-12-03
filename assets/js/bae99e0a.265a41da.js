"use strict";(self.webpackChunkstarlake=self.webpackChunkstarlake||[]).push([[7430],{3905:(e,t,a)=>{a.d(t,{Zo:()=>u,kt:()=>h});var n=a(67294);function r(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function o(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function s(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?o(Object(a),!0).forEach((function(t){r(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):o(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function l(e,t){if(null==e)return{};var a,n,r=function(e,t){if(null==e)return{};var a,n,r={},o=Object.keys(e);for(n=0;n<o.length;n++)a=o[n],t.indexOf(a)>=0||(r[a]=e[a]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(n=0;n<o.length;n++)a=o[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(r[a]=e[a])}return r}var i=n.createContext({}),d=function(e){var t=n.useContext(i),a=t;return e&&(a="function"==typeof e?e(t):s(s({},t),e)),a},u=function(e){var t=d(e.components);return n.createElement(i.Provider,{value:t},e.children)},p="mdxType",c={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},m=n.forwardRef((function(e,t){var a=e.components,r=e.mdxType,o=e.originalType,i=e.parentName,u=l(e,["components","mdxType","originalType","parentName"]),p=d(a),m=r,h=p["".concat(i,".").concat(m)]||p[m]||c[m]||o;return a?n.createElement(h,s(s({ref:t},u),{},{components:a})):n.createElement(h,s({ref:t},u))}));function h(e,t){var a=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=a.length,s=new Array(o);s[0]=m;var l={};for(var i in t)hasOwnProperty.call(t,i)&&(l[i]=t[i]);l.originalType=e,l[p]="string"==typeof e?e:r,s[1]=l;for(var d=2;d<o;d++)s[d]=a[d];return n.createElement.apply(null,s)}return n.createElement.apply(null,a)}m.displayName="MDXCreateElement"},85162:(e,t,a)=>{a.d(t,{Z:()=>s});var n=a(67294),r=a(86010);const o={tabItem:"tabItem_Ymn6"};function s(e){let{children:t,hidden:a,className:s}=e;return n.createElement("div",{role:"tabpanel",className:(0,r.Z)(o.tabItem,s),hidden:a},t)}},74866:(e,t,a)=>{a.d(t,{Z:()=>w});var n=a(87462),r=a(67294),o=a(86010),s=a(12466),l=a(16550),i=a(91980),d=a(67392),u=a(50012);function p(e){return function(e){return r.Children.map(e,(e=>{if(!e||(0,r.isValidElement)(e)&&function(e){const{props:t}=e;return!!t&&"object"==typeof t&&"value"in t}(e))return e;throw new Error(`Docusaurus error: Bad <Tabs> child <${"string"==typeof e.type?e.type:e.type.name}>: all children of the <Tabs> component should be <TabItem>, and every <TabItem> should have a unique "value" prop.`)}))?.filter(Boolean)??[]}(e).map((e=>{let{props:{value:t,label:a,attributes:n,default:r}}=e;return{value:t,label:a,attributes:n,default:r}}))}function c(e){const{values:t,children:a}=e;return(0,r.useMemo)((()=>{const e=t??p(a);return function(e){const t=(0,d.l)(e,((e,t)=>e.value===t.value));if(t.length>0)throw new Error(`Docusaurus error: Duplicate values "${t.map((e=>e.value)).join(", ")}" found in <Tabs>. Every value needs to be unique.`)}(e),e}),[t,a])}function m(e){let{value:t,tabValues:a}=e;return a.some((e=>e.value===t))}function h(e){let{queryString:t=!1,groupId:a}=e;const n=(0,l.k6)(),o=function(e){let{queryString:t=!1,groupId:a}=e;if("string"==typeof t)return t;if(!1===t)return null;if(!0===t&&!a)throw new Error('Docusaurus error: The <Tabs> component groupId prop is required if queryString=true, because this value is used as the search param name. You can also provide an explicit value such as queryString="my-search-param".');return a??null}({queryString:t,groupId:a});return[(0,i._X)(o),(0,r.useCallback)((e=>{if(!o)return;const t=new URLSearchParams(n.location.search);t.set(o,e),n.replace({...n.location,search:t.toString()})}),[o,n])]}function f(e){const{defaultValue:t,queryString:a=!1,groupId:n}=e,o=c(e),[s,l]=(0,r.useState)((()=>function(e){let{defaultValue:t,tabValues:a}=e;if(0===a.length)throw new Error("Docusaurus error: the <Tabs> component requires at least one <TabItem> children component");if(t){if(!m({value:t,tabValues:a}))throw new Error(`Docusaurus error: The <Tabs> has a defaultValue "${t}" but none of its children has the corresponding value. Available values are: ${a.map((e=>e.value)).join(", ")}. If you intend to show no default tab, use defaultValue={null} instead.`);return t}const n=a.find((e=>e.default))??a[0];if(!n)throw new Error("Unexpected error: 0 tabValues");return n.value}({defaultValue:t,tabValues:o}))),[i,d]=h({queryString:a,groupId:n}),[p,f]=function(e){let{groupId:t}=e;const a=function(e){return e?`docusaurus.tab.${e}`:null}(t),[n,o]=(0,u.Nk)(a);return[n,(0,r.useCallback)((e=>{a&&o.set(e)}),[a,o])]}({groupId:n}),k=(()=>{const e=i??p;return m({value:e,tabValues:o})?e:null})();(0,r.useLayoutEffect)((()=>{k&&l(k)}),[k]);return{selectedValue:s,selectValue:(0,r.useCallback)((e=>{if(!m({value:e,tabValues:o}))throw new Error(`Can't select invalid tab value=${e}`);l(e),d(e),f(e)}),[d,f,o]),tabValues:o}}var k=a(72389);const g={tabList:"tabList__CuJ",tabItem:"tabItem_LNqP"};function y(e){let{className:t,block:a,selectedValue:l,selectValue:i,tabValues:d}=e;const u=[],{blockElementScrollPositionUntilNextRender:p}=(0,s.o5)(),c=e=>{const t=e.currentTarget,a=u.indexOf(t),n=d[a].value;n!==l&&(p(t),i(n))},m=e=>{let t=null;switch(e.key){case"Enter":c(e);break;case"ArrowRight":{const a=u.indexOf(e.currentTarget)+1;t=u[a]??u[0];break}case"ArrowLeft":{const a=u.indexOf(e.currentTarget)-1;t=u[a]??u[u.length-1];break}}t?.focus()};return r.createElement("ul",{role:"tablist","aria-orientation":"horizontal",className:(0,o.Z)("tabs",{"tabs--block":a},t)},d.map((e=>{let{value:t,label:a,attributes:s}=e;return r.createElement("li",(0,n.Z)({role:"tab",tabIndex:l===t?0:-1,"aria-selected":l===t,key:t,ref:e=>u.push(e),onKeyDown:m,onClick:c},s,{className:(0,o.Z)("tabs__item",g.tabItem,s?.className,{"tabs__item--active":l===t})}),a??t)})))}function b(e){let{lazy:t,children:a,selectedValue:n}=e;const o=(Array.isArray(a)?a:[a]).filter(Boolean);if(t){const e=o.find((e=>e.props.value===n));return e?(0,r.cloneElement)(e,{className:"margin-top--md"}):null}return r.createElement("div",{className:"margin-top--md"},o.map(((e,t)=>(0,r.cloneElement)(e,{key:t,hidden:e.props.value!==n}))))}function X(e){const t=f(e);return r.createElement("div",{className:(0,o.Z)("tabs-container",g.tabList)},r.createElement(y,(0,n.Z)({},e,t)),r.createElement(b,(0,n.Z)({},e,t)))}function w(e){const t=(0,k.Z)();return r.createElement(X,(0,n.Z)({key:String(t)},e))}},75955:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>u,contentTitle:()=>i,default:()=>h,frontMatter:()=>l,metadata:()=>d,toc:()=>p});var n=a(87462),r=(a(67294),a(3905)),o=a(74866),s=a(85162);const l={},i="Load",d={unversionedId:"user-guide/load",id:"version-1.0.0/user-guide/load",title:"Load",description:"In this section you will learn how to load and transform data using the samples files created when bootstrapping a new project and the [schemas",source:"@site/versioned_docs/version-1.0.0/0400-user-guide/100.load.mdx",sourceDirName:"0400-user-guide",slug:"/user-guide/load",permalink:"/starlake/docs/user-guide/load",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/versioned_docs/version-1.0.0/0400-user-guide/100.load.mdx",tags:[],version:"1.0.0",sidebarPosition:100,frontMatter:{},sidebar:"starlakeSidebar",previous:{title:"Extract",permalink:"/starlake/docs/user-guide/extract"},next:{title:"Transform",permalink:"/starlake/docs/user-guide/transform"}},u={},p=[{value:"Load files",id:"load-files",level:2},{value:"Import step",id:"import-step",level:3},{value:"Load step",id:"load-step",level:3},{value:"Check the result",id:"check-the-result",level:3},{value:"Incremental updates",id:"incremental-updates",level:2},{value:"Targeting another datawarehouse",id:"targeting-another-datawarehouse",level:2}],c={toc:p},m="wrapper";function h(e){let{components:t,...a}=e;return(0,r.kt)(m,(0,n.Z)({},c,a,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"load"},"Load"),(0,r.kt)("p",null,"In this section you will learn how to load and transform data using the samples files created when ",(0,r.kt)("a",{parentName:"p",href:"bootstrap"},"bootstrapping a new project")," and the ",(0,r.kt)("a",{parentName:"p",href:"infer-schema"},"schemas\ninferred")," from these files."),(0,r.kt)("h2",{id:"load-files"},"Load files"),(0,r.kt)("p",null,"Loading is a two step process: The optional ",(0,r.kt)("inlineCode",{parentName:"p"},"import")," step and the ",(0,r.kt)("inlineCode",{parentName:"p"},"load")," step."),(0,r.kt)("h3",{id:"import-step"},"Import step"),(0,r.kt)("p",null,"In this first step, Starlake will look at the ",(0,r.kt)("em",{parentName:"p"},"directory")," attribute value in the YAML files and look at the file that matches the expected patterns defined in the table definition files.\nIn our example, the directories are ",(0,r.kt)("inlineCode",{parentName:"p"},"{{incoming_path}}/sales")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"{{incoming_path}}/hr"),"."),(0,r.kt)("p",null,"the ",(0,r.kt)("inlineCode",{parentName:"p"},"import")," command moves the files that satisfy one table pattern from the ",(0,r.kt)("em",{parentName:"p"},"incoming")," folder to the ",(0,r.kt)("inlineCode",{parentName:"p"},"datasets/pending")," folder.\nFiles that do not satisfy any pattern won't be loaded and are moved to the ",(0,r.kt)("inlineCode",{parentName:"p"},"datasets/unresolved")," directory."),(0,r.kt)(o.Z,{groupId:"platforms",mdxType:"Tabs"},(0,r.kt)(s.Z,{value:"linux_macos",label:"Linux/MacOS",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-sh"},"$ cd $HOME/userguide\n$ starlake import\n"))),(0,r.kt)(s.Z,{value:"windows",label:"Windows",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-powershell"},"c:\\users\\me\\userguide> starlake import\n"))),(0,r.kt)(s.Z,{value:"docker",label:"Docker",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-shell"},"$ docker run                                                  \\\n    -e SL_ROOT=/app/userguide                                \\\n    -v $HOME/userguide:/app/userguide -it starlake import\n")))),(0,r.kt)("p",null,"The sample data files has now been moved to the ",(0,r.kt)("inlineCode",{parentName:"p"},"userguide/datasets/pending/sales")," directory."),(0,r.kt)("admonition",{type:"note"},(0,r.kt)("p",{parentName:"admonition"},"This step is optional and does not need to be run if your files directly arrive in the ",(0,r.kt)("inlineCode",{parentName:"p"},"datasets/pending/sales")," folder."),(0,r.kt)("p",{parentName:"admonition"},"Also note that the pending directory  (",(0,r.kt)("inlineCode",{parentName:"p"},"${SL_ROOT}/datasets/pending")," by default) location may be ",(0,r.kt)("a",{parentName:"p",href:"../configuration/filesystem"},"redefined"),".")),(0,r.kt)("h3",{id:"load-step"},"Load step"),(0,r.kt)("p",null,"In this second step, each line in the file present in the ",(0,r.kt)("inlineCode",{parentName:"p"},"datasets/pending")," folder is checked against the schema described in the YAML file\nand its result is stored in the warehouse."),(0,r.kt)(o.Z,{groupId:"platforms",mdxType:"Tabs"},(0,r.kt)(s.Z,{value:"linux_macos",label:"Linux/MacOS",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-sh"},"$ cd $HOME/userguide\n$ starlake load\n"))),(0,r.kt)(s.Z,{value:"windows",label:"Windows",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-powershell"},"c:\\users\\me\\starlake> starlake load\n"))),(0,r.kt)(s.Z,{value:"docker",label:"Docker",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-shell"},"$ docker run                                                      \\\n    -e SL_ROOT=/app/userguide                                    \\\n    -v $HOME/myproject:/app/userguide -it starlake load\n")))),(0,r.kt)("p",null,"This will load the data files and since we chose the localFilesystem connection, they will be stored them as parquet files into the following folders:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"datasets/accepted")," for valid records"),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"datasets/rejected")," for invalid records"),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"datasets/unresolved")," for unrecognized files")),(0,r.kt)("admonition",{type:"caution"},(0,r.kt)("p",{parentName:"admonition"},"Starlake validate the data against the table's schema of the first pattern that match with the file name.\nHence, you must be careful regarding the pattern you set.\nMake sure that there is no overlap.")),(0,r.kt)("h3",{id:"check-the-result"},"Check the result"),(0,r.kt)("p",null,"You can check the result by running the following python script to read the parquet file from the project directory:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-python"},"import pandas as pd\nfilepath = 'datasets/accepted/sales/customers/'\ndf = pd.read_parquet(filepath)\ndf.head\n")),(0,r.kt)("h2",{id:"incremental-updates"},"Incremental updates"),(0,r.kt)("p",null,"Sometimes, we want to alter existing data by adding new records or updating existing ones. This is called an incremental update or upsert."),(0,r.kt)("p",null,"This is useful when we have a large amount of data and we want to avoid reloading the entire data set every time we want to update our data warehouse."),(0,r.kt)("p",null,"In this section, we will see how to do incremental updates."),(0,r.kt)("p",null,"We've seen that we may overwrite or append the data in a table when we load it, but what if for some existing records,\nwe want to update the data in a table ?"),(0,r.kt)("p",null,"This is done using the ",(0,r.kt)("inlineCode",{parentName:"p"},"merge")," attribute in the table configuration file. In this example, we will use the\n",(0,r.kt)("inlineCode",{parentName:"p"},"merge")," attribute to upsert records in the ",(0,r.kt)("inlineCode",{parentName:"p"},"sales.customers"),"."),(0,r.kt)("p",null,"The ",(0,r.kt)("inlineCode",{parentName:"p"},"merge.timestamp")," attribute is used to recognize the most recent record concerning a customer identified by the columns listed in ",(0,r.kt)("inlineCode",{parentName:"p"},"merge.key")," . If not specified, new records (the records being loaded) are considered\nto be the most recent ones. In our case, the ",(0,r.kt)("inlineCode",{parentName:"p"},"signup")," date will used as the merge timestamp for the records identified by their column ",(0,r.kt)("inlineCode",{parentName:"p"},"id"),"."),(0,r.kt)("p",null,"This is the content of the ",(0,r.kt)("inlineCode",{parentName:"p"},"metadata/load/customers.sl.yml")," file once the ",(0,r.kt)("inlineCode",{parentName:"p"},"merge")," attribute is added (highlighted lines):"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="metadata/load/sales/customers.sl.yml" {3-5}',title:'"metadata/load/sales/customers.sl.yml"',"{3-5}":!0},'---\ntable:\n  merge:\n    timestamp: signup\n    key: [id]\n  pattern: "customers.*.psv" # This property is a regular expression that will be used to match the file name.\n                             # Please replace it by the adequate file pattern eq. customers-.*.psv if required\n  attributes:         # Description of the fields to recognize\n    - name: "id"        # attribute name and column name in the destination table if no rename attribute is defined\n      type: "string"    # expected type\n      array: false      # is it an array (false by default, ignored in DSV files) ?\n      required: false   # Is this field required in the source (false by default, change it accordingly) ?\n      privacy: "NONE"   # Should we encrypt this field before loading to the warehouse (No encryption by default )?\n      ignore: false     # Should this field be excluded (false by default) ?\n    - name: "signup"    # second attribute\n      type: "timestamp" # recognized type by analyzing input.\n    - name: "contact"\n      type: "string"\n      # ...\n    - name: "birthdate"\n      type: "date"      # recognized as semantic type date.\n      # ...\n    - name: "firstname"\n      type: "string"\n      # ...\n    - name: "lastname"\n      type: "string"\n      # ...\n    - name: "country"\n      type: "string"\n      # ...               # and so on ...\n  metadata:\n    mode: "FILE"\n    format: "DSV"         # detected format\n    encoding: "UTF-8"\n    multiline: false\n    array: false\n    withHeader: true\n    separator: "|"        # detected separator\n    quote: "\\""\n    escape: "\\\\"\n    write: "APPEND"\n\n')),(0,r.kt)("h2",{id:"targeting-another-datawarehouse"},"Targeting another datawarehouse"),(0,r.kt)("p",null,"We just loaded our text file as a parquet file. This is a very common format for data scientists and analysts.\nThrough minimum extra configuration, we are able to run Starlake on top of any warehouse and have these datasets available as tables."),(0,r.kt)("p",null,"The examples below describe the extra configuration required in the ",(0,r.kt)("inlineCode",{parentName:"p"},"metadata/application.sl.yml")," configuration file\nto load the data into  bigquery, databricks, redshift, snowflake or postgresql."),(0,r.kt)(o.Z,{groupId:"datawarehouse",mdxType:"Tabs"},(0,r.kt)(s.Z,{value:"bigquery",label:"bigquery",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'application:\n  connectionRef: "bigquery"\n  loader: native\n  connections:\n    bigquery:\n      type: "bigquery"\n      options:\n        location: "us-central1" # europe-west1 or EU or US or ..\n        authType: "APPLICATION_DEFAULT"\n        authScopes: "https://www.googleapis.com/auth/cloud-platform" # comma separated list of scopes\n        #authType: SERVICE_ACCOUNT_JSON_KEYFILE\n        #jsonKeyfile: "/Users/me/.gcloud/keys/starlake-me.json"\n        #authType: "ACCESS_TOKEN"\n        #gcpAccessToken: "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"\n'))),(0,r.kt)(s.Z,{value:"databricks",label:"databricks",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'application:\n  connectionRef: "databricks"\n  loader: spark # note the spark loader here\n  connections:\n    localFilesystem:\n      type: "databricks"\n'))),(0,r.kt)(s.Z,{value:"postgresql",label:"postgresql",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'application:\n  connectionRef: "postgresql"\n  loader: spark # note the spark loader here\n  connections:\n    postgresql:\n      type: jdbc\n      options:\n        url: "jdbc:postgresql://{{POSTGRES_HOST}}:{{POSTGRES_PORT}}/{{POSTGRES_DATABASE}}"\n        driver: "org.postgresql.Driver"\n        supportTruncateOnInsert: false\n        user: "{{DATABASE_USER}}"\n        password: "{{DATABASE_PASSWORD}}"\n        quoteIdentifiers: false\n'))),(0,r.kt)(s.Z,{value:"redshift",label:"Redshift",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'application:\n  connectionRef: "redshift"\n  loader: spark # note the spark loader here\n  connections:\n    redshift:\n      options:\n        url: "jdbc:redshift://account.region.redshift.amazonaws.com:5439/database",\n        driver: com.amazon.redshift.Driver\n        password: "{{REDSHIFT_PASSWORD}}"\n        tempdir: "s3a://bucketName/data",\n        tempdir_region: "eu-central-1" # required only if running from outside AWS (your laptop ...)\n        aws_iam_role: "arn:aws:iam::aws_count_id:role/role_name"\n')),(0,r.kt)("p",null,"Make sure to set the following environment variables:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-shell"},"\nexport AWS_ACCESS_KEY_ID= # your AWS access key\nexport AWS_SECRET_ACCESS_KEY= # your AWS secret key\nexport AWS_SESSION_TOKEN= # your AWS session token\nexport AWS_DEFAULT_REGION= # your AWS region\n\n"))),(0,r.kt)(s.Z,{value:"snowflake",label:"snowflake",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'application:\n  connectionRef: "snowflake"\n  loader: native\n  connections:\n    snowflake:\n      type: jdbc\n      options:\n        url: "jdbc:snowflake://{{SNOWFLAKE_ACCOUNT}}.snowflakecomputing.com/"\n        driver: "net.snowflake.client.jdbc.SnowflakeDriver"\n        user: "{{SNOWFLAKE_USER}}"\n        password: "{{SNOWFLAKE_PASSWORD}}"\n        warehouse: "{{SNOWFLAKE_WAREHOUSE}}"\n        db: "{{SNOWFLAKE_DB}}"\n        keep_column_case: "off"\n        preactions: "alter session set TIMESTAMP_TYPE_MAPPING = \'TIMESTAMP_LTZ\';ALTER SESSION SET QUOTED_IDENTIFIERS_IGNORE_CASE = true"\n'))),(0,r.kt)(s.Z,{value:"spark-bigquery",label:"spark bigquery",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'application:\n  connectionRef: "spark-bigquery"\n  loader: spark # note the spark loader here\n  connections:\n    spark-bigquery:\n      type: "bigquery"\n      options:\n        location: "EU" # EU or US or ..\n        authType: "APPLICATION_DEFAULT"\n        authScopes: "https://www.googleapis.com/auth/cloud-platform" # comma separated list of scopes\n        writeMethod: "direct" # direct or indirect (indirect is required for certain features see https://github.com/GoogleCloudDataproc/spark-bigquery-connector)\n        #gcsBucket: "starlake-app" # Temporary GCS Bucket where intermediary files will be stored. Required in indirect mode only\n        #authType: SERVICE_ACCOUNT_JSON_KEYFILE\n        #jsonKeyfile: "/Users/me/.gcloud/keys/starlake-me.json"\n        #authType: "ACCESS_TOKEN"\n        #gcpAccessToken: "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"\n'))),(0,r.kt)(s.Z,{value:"spark-redshift",label:"Spark Redshift",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'application:\n  connectionRef: "redshift"\n  loader: spark # note the spark loader here\n  connections:\n    redshift:\n      sparkFormat: "io.github.spark_redshift_community.spark.redshift" # if running on top of Spark or else  "redshift" if running on top of Databricks\n      options:\n        url: "jdbc:redshift://account.region.redshift.amazonaws.com:5439/database",\n        driver: com.amazon.redshift.Driver\n        password: "{{REDSHIFT_PASSWORD}}"\n        tempdir: "s3a://bucketName/data",\n        tempdir_region: "eu-central-1" # required only if running from outside AWS (your laptop ...)\n        aws_iam_role: "arn:aws:iam::aws_count_id:role/role_name"\n')),(0,r.kt)("p",null,"Make sure to set the following environment variables:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-shell"},"\nexport AWS_ACCESS_KEY_ID= # your AWS access key\nexport AWS_SECRET_ACCESS_KEY= # your AWS secret key\nexport AWS_SESSION_TOKEN= # your AWS session token\nexport AWS_DEFAULT_REGION= # your AWS region\n\n"))),(0,r.kt)(s.Z,{value:"spark-snowflake",label:"spark snowflake",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'application:\n  connectionRef: "spark-snowflake"\n  loader: spark # note the spark loader here\n  connections:\n      spark-snowflake:\n        type: jdbc\n        sparkFormat: snowflake\n        options:\n          sfUrl: "{{SNOWFLAKE_ACCOUNT}}.snowflakecomputing.com" # make sure you do not prefix by jdbc:snowflake://. This is done by the snowflaek driver\n          #sfDriver: "net.snowflake.client.jdbc.SnowflakeDriver"\n          sfUser: "{{SNOWFLAKE_USER}}"\n          sfPassword: "{{SNOWFLAKE_PASSWORD}}"\n          sfWarehouse: "{{SNOWFLAKE_WAREHOUSE}}"\n          sfDatabase: "{{SNOWFLAKE_DB}}"\n          keep_column_case: "off"\n          autopushdown: on\n          preactions: "alter session set TIMESTAMP_TYPE_MAPPING = \'TIMESTAMP_LTZ\';ALTER SESSION SET QUOTED_IDENTIFIERS_IGNORE_CASE = true"\n\n')))),(0,r.kt)("p",null,"Using Spark instead of the BigQuery Load API may slow down the ingestion process but it has among others the following advantages:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"It allows to load data from any source supported by Spark including Fixed Width Files, XML files, JSON Arrays files ..."),(0,r.kt)("li",{parentName:"ul"},"It allows to load data into any destination supported by Spark including Snowflake, Amazon Redshift ..."),(0,r.kt)("li",{parentName:"ul"},"It allows to apply any transformation supported by Spark"),(0,r.kt)("li",{parentName:"ul"},"It allows to report any number of errors instead of 5 errors max with the BigQuery Load API (This is a BigQuery API Limitation)")),(0,r.kt)("p",null,"To load the data into BigQuery, simply put back the samples data files now archived in the datasets folder back\nto the sample-data/hr and sample-data/sales folders and run the ",(0,r.kt)("em",{parentName:"p"},"import")," and ",(0,r.kt)("em",{parentName:"p"},"load")," commands again."),(0,r.kt)("admonition",{type:"note"},(0,r.kt)("p",{parentName:"admonition"},"Spark is not required to load data into any of the target datawarehouse, nor is it required to run transformations on the data."),(0,r.kt)("p",{parentName:"admonition"},"Even when using Spark, you do not need to instantiate a cluster.  Spark becomes useful for advanced data validation at load time.")))}h.isMDXComponent=!0}}]);