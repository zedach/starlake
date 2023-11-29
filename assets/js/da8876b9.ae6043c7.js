"use strict";(self.webpackChunkstarlake=self.webpackChunkstarlake||[]).push([[1762],{3905:(e,n,t)=>{t.d(n,{Zo:()=>u,kt:()=>m});var a=t(67294);function r(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function o(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);n&&(a=a.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,a)}return t}function l(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?o(Object(t),!0).forEach((function(n){r(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):o(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function s(e,n){if(null==e)return{};var t,a,r=function(e,n){if(null==e)return{};var t,a,r={},o=Object.keys(e);for(a=0;a<o.length;a++)t=o[a],n.indexOf(t)>=0||(r[t]=e[t]);return r}(e,n);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)t=o[a],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(r[t]=e[t])}return r}var i=a.createContext({}),c=function(e){var n=a.useContext(i),t=n;return e&&(t="function"==typeof e?e(n):l(l({},n),e)),t},u=function(e){var n=c(e.components);return a.createElement(i.Provider,{value:n},e.children)},p="mdxType",d={inlineCode:"code",wrapper:function(e){var n=e.children;return a.createElement(a.Fragment,{},n)}},X=a.forwardRef((function(e,n){var t=e.components,r=e.mdxType,o=e.originalType,i=e.parentName,u=s(e,["components","mdxType","originalType","parentName"]),p=c(t),X=r,m=p["".concat(i,".").concat(X)]||p[X]||d[X]||o;return t?a.createElement(m,l(l({ref:n},u),{},{components:t})):a.createElement(m,l({ref:n},u))}));function m(e,n){var t=arguments,r=n&&n.mdxType;if("string"==typeof e||r){var o=t.length,l=new Array(o);l[0]=X;var s={};for(var i in n)hasOwnProperty.call(n,i)&&(s[i]=n[i]);s.originalType=e,s[p]="string"==typeof e?e:r,l[1]=s;for(var c=2;c<o;c++)l[c]=t[c];return a.createElement.apply(null,l)}return a.createElement.apply(null,t)}X.displayName="MDXCreateElement"},85162:(e,n,t)=>{t.d(n,{Z:()=>l});var a=t(67294),r=t(86010);const o={tabItem:"tabItem_Ymn6"};function l(e){let{children:n,hidden:t,className:l}=e;return a.createElement("div",{role:"tabpanel",className:(0,r.Z)(o.tabItem,l),hidden:t},n)}},74866:(e,n,t)=>{t.d(n,{Z:()=>S});var a=t(87462),r=t(67294),o=t(86010),l=t(12466),s=t(16550),i=t(91980),c=t(67392),u=t(50012);function p(e){return function(e){return r.Children.map(e,(e=>{if(!e||(0,r.isValidElement)(e)&&function(e){const{props:n}=e;return!!n&&"object"==typeof n&&"value"in n}(e))return e;throw new Error(`Docusaurus error: Bad <Tabs> child <${"string"==typeof e.type?e.type:e.type.name}>: all children of the <Tabs> component should be <TabItem>, and every <TabItem> should have a unique "value" prop.`)}))?.filter(Boolean)??[]}(e).map((e=>{let{props:{value:n,label:t,attributes:a,default:r}}=e;return{value:n,label:t,attributes:a,default:r}}))}function d(e){const{values:n,children:t}=e;return(0,r.useMemo)((()=>{const e=n??p(t);return function(e){const n=(0,c.l)(e,((e,n)=>e.value===n.value));if(n.length>0)throw new Error(`Docusaurus error: Duplicate values "${n.map((e=>e.value)).join(", ")}" found in <Tabs>. Every value needs to be unique.`)}(e),e}),[n,t])}function X(e){let{value:n,tabValues:t}=e;return t.some((e=>e.value===n))}function m(e){let{queryString:n=!1,groupId:t}=e;const a=(0,s.k6)(),o=function(e){let{queryString:n=!1,groupId:t}=e;if("string"==typeof n)return n;if(!1===n)return null;if(!0===n&&!t)throw new Error('Docusaurus error: The <Tabs> component groupId prop is required if queryString=true, because this value is used as the search param name. You can also provide an explicit value such as queryString="my-search-param".');return t??null}({queryString:n,groupId:t});return[(0,i._X)(o),(0,r.useCallback)((e=>{if(!o)return;const n=new URLSearchParams(a.location.search);n.set(o,e),a.replace({...a.location,search:n.toString()})}),[o,a])]}function f(e){const{defaultValue:n,queryString:t=!1,groupId:a}=e,o=d(e),[l,s]=(0,r.useState)((()=>function(e){let{defaultValue:n,tabValues:t}=e;if(0===t.length)throw new Error("Docusaurus error: the <Tabs> component requires at least one <TabItem> children component");if(n){if(!X({value:n,tabValues:t}))throw new Error(`Docusaurus error: The <Tabs> has a defaultValue "${n}" but none of its children has the corresponding value. Available values are: ${t.map((e=>e.value)).join(", ")}. If you intend to show no default tab, use defaultValue={null} instead.`);return n}const a=t.find((e=>e.default))??t[0];if(!a)throw new Error("Unexpected error: 0 tabValues");return a.value}({defaultValue:n,tabValues:o}))),[i,c]=m({queryString:t,groupId:a}),[p,f]=function(e){let{groupId:n}=e;const t=function(e){return e?`docusaurus.tab.${e}`:null}(n),[a,o]=(0,u.Nk)(t);return[a,(0,r.useCallback)((e=>{t&&o.set(e)}),[t,o])]}({groupId:a}),b=(()=>{const e=i??p;return X({value:e,tabValues:o})?e:null})();(0,r.useLayoutEffect)((()=>{b&&s(b)}),[b]);return{selectedValue:l,selectValue:(0,r.useCallback)((e=>{if(!X({value:e,tabValues:o}))throw new Error(`Can't select invalid tab value=${e}`);s(e),c(e),f(e)}),[c,f,o]),tabValues:o}}var b=t(72389);const k={tabList:"tabList__CuJ",tabItem:"tabItem_LNqP"};function y(e){let{className:n,block:t,selectedValue:s,selectValue:i,tabValues:c}=e;const u=[],{blockElementScrollPositionUntilNextRender:p}=(0,l.o5)(),d=e=>{const n=e.currentTarget,t=u.indexOf(n),a=c[t].value;a!==s&&(p(n),i(a))},X=e=>{let n=null;switch(e.key){case"Enter":d(e);break;case"ArrowRight":{const t=u.indexOf(e.currentTarget)+1;n=u[t]??u[0];break}case"ArrowLeft":{const t=u.indexOf(e.currentTarget)-1;n=u[t]??u[u.length-1];break}}n?.focus()};return r.createElement("ul",{role:"tablist","aria-orientation":"horizontal",className:(0,o.Z)("tabs",{"tabs--block":t},n)},c.map((e=>{let{value:n,label:t,attributes:l}=e;return r.createElement("li",(0,a.Z)({role:"tab",tabIndex:s===n?0:-1,"aria-selected":s===n,key:n,ref:e=>u.push(e),onKeyDown:X,onClick:d},l,{className:(0,o.Z)("tabs__item",k.tabItem,l?.className,{"tabs__item--active":s===n})}),t??n)})))}function g(e){let{lazy:n,children:t,selectedValue:a}=e;const o=(Array.isArray(t)?t:[t]).filter(Boolean);if(n){const e=o.find((e=>e.props.value===a));return e?(0,r.cloneElement)(e,{className:"margin-top--md"}):null}return r.createElement("div",{className:"margin-top--md"},o.map(((e,n)=>(0,r.cloneElement)(e,{key:n,hidden:e.props.value!==a}))))}function h(e){const n=f(e);return r.createElement("div",{className:(0,o.Z)("tabs-container",k.tabList)},r.createElement(y,(0,a.Z)({},e,n)),r.createElement(g,(0,a.Z)({},e,n)))}function S(e){const n=(0,b.Z)();return r.createElement(h,(0,a.Z)({key:String(n)},e))}},66644:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>u,contentTitle:()=>i,default:()=>m,frontMatter:()=>s,metadata:()=>c,toc:()=>p});var a=t(87462),r=(t(67294),t(3905)),o=t(74866),l=t(85162);const s={},i="Connections",c={unversionedId:"configuration/connections",id:"configuration/connections",title:"Connections",description:"Connections are defined in the connections section under the root attribute application.",source:"@site/docs/0500-configuration/0110-connections.mdx",sourceDirName:"0500-configuration",slug:"/configuration/connections",permalink:"/starlake/docs/next/configuration/connections",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/docs/0500-configuration/0110-connections.mdx",tags:[],version:"current",sidebarPosition:110,frontMatter:{},sidebar:"starlakeSidebar",previous:{title:"Scheduling",permalink:"/starlake/docs/next/configuration/scheduling"},next:{title:"Filesystem",permalink:"/starlake/docs/next/configuration/filesystem"}},u={},p=[{value:"Local File System",id:"local-file-system",level:2},{value:"Google BigQuery",id:"google-bigquery",level:2},{value:"Apache Spark / Databricks",id:"apache-spark--databricks",level:2},{value:"Snowflake",id:"snowflake",level:2},{value:"Amazon Redshift",id:"amazon-redshift",level:2},{value:"Postgres",id:"postgres",level:2}],d={toc:p},X="wrapper";function m(e){let{components:n,...t}=e;return(0,r.kt)(X,(0,a.Z)({},d,t,{components:n,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"connections"},"Connections"),(0,r.kt)("p",null,"Connections are defined in the ",(0,r.kt)("inlineCode",{parentName:"p"},"connections")," section under the root attribute ",(0,r.kt)("inlineCode",{parentName:"p"},"application"),"."),(0,r.kt)("p",null,"The following types of connections are supported:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"#local-file-system"},"Local File System")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"#google-bigquery"},"BigQuery")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"#apache-spark--databricks"},"Spark / Databricks")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"#snowflake"},"Snowflake")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"#amazon-redshift"},"Redshift")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"#postgres"},"Postgres"))),(0,r.kt)("h2",{id:"local-file-system"},"Local File System"),(0,r.kt)("p",null,"The local file system connection is used to read and write files to the local file system."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},"application:\n    connections:\n    local:\n        type: local\n")),(0,r.kt)("p",null,"Files will be stored in the ",(0,r.kt)("inlineCode",{parentName:"p"},"area")," directory under the ",(0,r.kt)("inlineCode",{parentName:"p"},"datasets")," directory.\nD\xe9fault values for ",(0,r.kt)("inlineCode",{parentName:"p"},"area")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"datasets")," can be set in the ",(0,r.kt)("inlineCode",{parentName:"p"},"application")," section."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'application:\n    datasets = ${root}"/datasets"\n    datasets = ${?SL_DATASETS}\n    area:\n        pending: "pending" # Location where files of pending load are stored. May be overloaded by the ${SL_AREA_PENDING} environment variable.\n        unresolved: "unresolved" # Location where files that do not match any pattern are moved. May be overloaded by the ${SL_AREA_UNRESOLVED} environment variable.\n        archive: "archive" # Location where files are moved after they have been processed. May be overloaded by the ${SL_AREA_ARCHIVE} environment variable.\n        ingesting: "ingesting" # Location where files are moved while they are being processed. May be overloaded by the ${SL_AREA_INGESTING} environment variable.\n        accepted: "accepted" # Location where files are moved after they have been processed and accepted. May be overloaded by the ${SL_AREA_ACCEPTED} environment variable.\n        rejected: "rejected" # Location where files are moved after they have been processed and rejected. May be overloaded by the ${SL_AREA_REJECTED} environment variable.\n        business: "business" # Location where transform tasks store their result. May be overloaded by the ${SL_AREA_BUSINESS} environment variable.\n        replay: "replay" # Location rejected records are stored in their orginial format. May be overloaded by the ${SL_AREA_REPLAY} environment variable.\n        hiveDatabase: "${domain}_${area}" # Hive database name. May be overloaded by the ${SL_AREA_HIVE_DATABASE} environment variable.\n\n')),(0,r.kt)("h2",{id:"google-bigquery"},"Google BigQuery"),(0,r.kt)("p",null,"Starlake support native and spark / dataproc bigquery connections."),(0,r.kt)(o.Z,{groupId:"bq_connections",mdxType:"Tabs"},(0,r.kt)(l.Z,{label:"BigQuery",value:"bigquery",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'application:\n  connections:\n    bigquery:\n      type: "bigquery"\n      options:\n        location: "us-central1" # EU or US or ...\n        authType: "APPLICATION_DEFAULT"\n        authScopes: "https://www.googleapis.com/auth/cloud-platform" # comma separated list of scopes\n        #authType: SERVICE_ACCOUNT_JSON_KEYFILE\n        #jsonKeyfile: "/Users/me/.gcloud/keys/starlake-me.json"\n        #authType: "ACCESS_TOKEN"\n        #gcpAccessToken: "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"\n  accessPolicies: # Required when applying access policies to table columns (Column Level Security)\n    apply: true\n    location: EU\n    taxonomy: RGPD\n'))),(0,r.kt)(l.Z,{label:"Spark BigQuery Direct",value:"spark-bigquery-direct",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'application:\n  connections:\n    bigquery:\n      type: "bigquery"\n      options:\n        writeMethod: "direct" # direct or indirect (indirect is required for certain features see https://github.com/GoogleCloudDataproc/spark-bigquery-connector)\n        location: "us-central1" # EU or US or ...\n        authType: "APPLICATION_DEFAULT"\n        authScopes: "https://www.googleapis.com/auth/cloud-platform" # comma separated list of scopes\n        #authType: SERVICE_ACCOUNT_JSON_KEYFILE\n        #jsonKeyfile: "/Users/me/.gcloud/keys/starlake-me.json"\n        #authType: "ACCESS_TOKEN"\n        #gcpAccessToken: "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"\n  spark:\n    datasource:\n      bigquery:\n        allowFieldAddition: "true" # Allow schema updates. To disable, set it to false\n        allowFieldRelaxation: "true" # Allow schema updates. To disable, set it to false\n\n'))),(0,r.kt)(l.Z,{label:"Spark BigQuery Indirect",value:"spark-bigquery-indirect",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'application:\n  connections:\n    bigquery:\n      type: "bigquery"\n      options:\n        writeMethod: "indirect" # direct or indirect (indirect is required for certain features see https://github.com/GoogleCloudDataproc/spark-bigquery-connector)\n        gcsBucket: "starlake-app" # Temporary GCS Bucket where intermediary files will be stored. Required in indirect mode only\n        location: "us-central1" # EU or US or ...\n        authType: "APPLICATION_DEFAULT"\n        authScopes: "https://www.googleapis.com/auth/cloud-platform" # comma separated list of scopes\n        #authType: SERVICE_ACCOUNT_JSON_KEYFILE\n        #jsonKeyfile: "/Users/me/.gcloud/keys/starlake-me.json"\n        #authType: "ACCESS_TOKEN"\n        #gcpAccessToken: "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"\n  spark:\n    datasource:\n      bigquery:\n        allowFieldAddition: "true" # Allow schema updates. To disable, set it to false\n        allowFieldRelaxation: "true" # Allow schema updates. To disable, set it to false\n\n')))),(0,r.kt)("h2",{id:"apache-spark--databricks"},"Apache Spark / Databricks"),(0,r.kt)("p",null,"Spark connections are used to read and write data from Spark."),(0,r.kt)(o.Z,{groupId:"spark_connections",mdxType:"Tabs"},(0,r.kt)(l.Z,{label:"Spark Parquet",value:"spark-parquet",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'application:\n  connections:\n    spark:\n      type: "spark"\n      options:\n        # any spark configuration can be set here\n      \n'))),(0,r.kt)(l.Z,{label:"Spark Delta",value:"spark-delta",mdxType:"TabItem"},(0,r.kt)("p",null,"In addition to the connection defined below, please download the following jars: "),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"https://repo1.maven.org/maven2/io/delta/delta-spark_2.12"},"delta-spark_2.12-VERSION.jar")," and place it in the ",(0,r.kt)("inlineCode",{parentName:"li"},"bin/deps")," directory of the starlake directory."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"https://repo1.maven.org/maven2/io/delta/delta-storage"},"delta-storage_2.12-VERSION.jar")," and place it in the ",(0,r.kt)("inlineCode",{parentName:"li"},"bin/deps")," directory of the starlake directory.")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'application:\n  connections:\n    spark:\n      type: "spark"\n      options:\n        # any spark configuration can be set here\n  spark:\n    sql:\n      extensions: "io.delta.sql.DeltaSparkSessionExtension"\n      catalog:\n        spark_catalog: "org.apache.spark.sql.delta.catalog.DeltaCatalog"\n\n')))),(0,r.kt)("h2",{id:"snowflake"},"Snowflake"),(0,r.kt)(o.Z,{groupId:"snow_connections",mdxType:"Tabs"},(0,r.kt)(l.Z,{label:"Snowflake JDBC",value:"snow-jdbc",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'\napplication:\n  connectionRef: {{connection}}\n  connections:\n    snowflake:\n      type: jdbc\n      options:\n        url: "jdbc:snowflake://{{SNOWFLAKE_ACCOUNT}}.snowflakecomputing.com"\n        driver: "net.snowflake.client.jdbc.SnowflakeDriver"\n        user: {{SNOWFLAKE_USER}}\n        password: {{SNOWFLAKE_PASSWORD}}\n        warehouse: {{SNOWFLAKE_WAREHOUSE}}\n        db: {{SNOWFLAKE_DB}}\n        keep_column_case: "off"\n        preactions: "alter session set TIMESTAMP_TYPE_MAPPING = \'TIMESTAMP_LTZ\';ALTER SESSION SET QUOTED_IDENTIFIERS_IGNORE_CASE = true"\n      \n'))),(0,r.kt)(l.Z,{label:"Snowflake Spark",value:"snow-spark",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'\napplication:\n  connectionRef: {{connection}}\n  connections:\n    snowflake:\n    spark-snowflake:\n      type: jdbc\n      sparkFormat: snowflake\n      options:\n        sfUrl: "{{SNOWFLAKE_ACCOUNT}}.snowflakecomputing.com" # make sure you do not prefix by jdbc:snowflake://. This is done by the snowflaek driver\n        #sfDriver: "net.snowflake.client.jdbc.SnowflakeDriver"\n        sfUser: {{SNOWFLAKE_USER}}\n        sfPassword: {{SNOWFLAKE_PASSWORD}}\n        sfWarehouse: {{SNOWFLAKE_WAREHOUSE}}\n        sfDatabase: {{SNOWFLAKE_DB}}\n        keep_column_case: "off"\n        autopushdown: on\n        preactions: "alter session set TIMESTAMP_TYPE_MAPPING = \'TIMESTAMP_LTZ\';ALTER SESSION SET QUOTED_IDENTIFIERS_IGNORE_CASE = true"\n\n')))),(0,r.kt)("h2",{id:"amazon-redshift"},"Amazon Redshift"),(0,r.kt)(o.Z,{groupId:"redshift_connections",mdxType:"Tabs"},(0,r.kt)(l.Z,{label:"Redshift JDBC",value:"redshift-jdbc",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'\napplication:\n  connections:\n    redshift:\n      options:\n        url: "jdbc:redshift://account.region.redshift.amazonaws.com:5439/database",\n        driver: com.amazon.redshift.Driver\n        password: "{{REDSHIFT_PASSWORD}}"\n        tempdir: "s3a://bucketName/data",\n        tempdir_region: "eu-central-1" # required only if running from outside AWS (your laptop ...)\n        aws_iam_role: "arn:aws:iam::aws_count_id:role/role_name"\n  \n'))),(0,r.kt)(l.Z,{label:"Redshift Spark",value:"redshift-spark",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'\napplication:\n  connections:\n    redshift:\n      sparkFormat: "io.github.spark_redshift_community.spark.redshift" # if running on top of Spark or else  "redshift" if running on top of Databricks\n      options:\n        url: "jdbc:redshift://account.region.redshift.amazonaws.com:5439/database",\n        driver: com.amazon.redshift.Driver\n        password: "{{REDSHIFT_PASSWORD}}"\n        tempdir: "s3a://bucketName/data",\n        tempdir_region: "eu-central-1" # required only if running from outside AWS (your laptop ...)\n        aws_iam_role: "arn:aws:iam::aws_count_id:role/role_name"\n\n')))),(0,r.kt)("h2",{id:"postgres"},"Postgres"),(0,r.kt)(o.Z,{groupId:"pg_connections",mdxType:"Tabs"},(0,r.kt)(l.Z,{label:"Postgres JDBC",value:"postgres-jdbc",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'application:\n  connectionRef: "postgresql"\n  connections:\n    postgresql:\n      type: jdbc\n      options:\n        url: "jdbc:postgresql://{{POSTGRES_HOST}}:{{POSTGRES_PORT}}/{{POSTGRES_DATABASE}}"\n        driver: "org.postgresql.Driver"\n        supportTruncateOnInsert: false\n        user: "{{DATABASE_USER}}"\n        password: "{{DATABASE_PASSWORD}}"\n        quoteIdentifiers: false\n'))),(0,r.kt)(l.Z,{label:"Postgres Spark",value:"pg-spark",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'\napplication:\n  connectionRef: "postgresql"\n  connections:\n    postgresql:\n      type: jdbc\n      sparkFormat: jdbc\n      options:\n        url: "jdbc:postgresql://{{POSTGRES_HOST}}:{{POSTGRES_PORT}}/{{POSTGRES_DATABASE}}"\n        driver: "org.postgresql.Driver"\n        supportTruncateOnInsert: false\n        user: "{{DATABASE_USER}}"\n        password: "{{DATABASE_PASSWORD}}"\n        quoteIdentifiers: false\n')))))}m.isMDXComponent=!0}}]);