"use strict";(self.webpackChunkstarlake=self.webpackChunkstarlake||[]).push([[9498],{3905:(e,t,a)=>{a.d(t,{Zo:()=>c,kt:()=>k});var n=a(67294);function r(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function l(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function i(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?l(Object(a),!0).forEach((function(t){r(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):l(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function o(e,t){if(null==e)return{};var a,n,r=function(e,t){if(null==e)return{};var a,n,r={},l=Object.keys(e);for(n=0;n<l.length;n++)a=l[n],t.indexOf(a)>=0||(r[a]=e[a]);return r}(e,t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(n=0;n<l.length;n++)a=l[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(r[a]=e[a])}return r}var s=n.createContext({}),u=function(e){var t=n.useContext(s),a=t;return e&&(a="function"==typeof e?e(t):i(i({},t),e)),a},c=function(e){var t=u(e.components);return n.createElement(s.Provider,{value:t},e.children)},p="mdxType",d={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},m=n.forwardRef((function(e,t){var a=e.components,r=e.mdxType,l=e.originalType,s=e.parentName,c=o(e,["components","mdxType","originalType","parentName"]),p=u(a),m=r,k=p["".concat(s,".").concat(m)]||p[m]||d[m]||l;return a?n.createElement(k,i(i({ref:t},c),{},{components:a})):n.createElement(k,i({ref:t},c))}));function k(e,t){var a=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var l=a.length,i=new Array(l);i[0]=m;var o={};for(var s in t)hasOwnProperty.call(t,s)&&(o[s]=t[s]);o.originalType=e,o[p]="string"==typeof e?e:r,i[1]=o;for(var u=2;u<l;u++)i[u]=a[u];return n.createElement.apply(null,i)}return n.createElement.apply(null,a)}m.displayName="MDXCreateElement"},85162:(e,t,a)=>{a.d(t,{Z:()=>i});var n=a(67294),r=a(86010);const l={tabItem:"tabItem_Ymn6"};function i(e){let{children:t,hidden:a,className:i}=e;return n.createElement("div",{role:"tabpanel",className:(0,r.Z)(l.tabItem,i),hidden:a},t)}},74866:(e,t,a)=>{a.d(t,{Z:()=>N});var n=a(87462),r=a(67294),l=a(86010),i=a(12466),o=a(16550),s=a(91980),u=a(67392),c=a(50012);function p(e){return function(e){return r.Children.map(e,(e=>{if(!e||(0,r.isValidElement)(e)&&function(e){const{props:t}=e;return!!t&&"object"==typeof t&&"value"in t}(e))return e;throw new Error(`Docusaurus error: Bad <Tabs> child <${"string"==typeof e.type?e.type:e.type.name}>: all children of the <Tabs> component should be <TabItem>, and every <TabItem> should have a unique "value" prop.`)}))?.filter(Boolean)??[]}(e).map((e=>{let{props:{value:t,label:a,attributes:n,default:r}}=e;return{value:t,label:a,attributes:n,default:r}}))}function d(e){const{values:t,children:a}=e;return(0,r.useMemo)((()=>{const e=t??p(a);return function(e){const t=(0,u.l)(e,((e,t)=>e.value===t.value));if(t.length>0)throw new Error(`Docusaurus error: Duplicate values "${t.map((e=>e.value)).join(", ")}" found in <Tabs>. Every value needs to be unique.`)}(e),e}),[t,a])}function m(e){let{value:t,tabValues:a}=e;return a.some((e=>e.value===t))}function k(e){let{queryString:t=!1,groupId:a}=e;const n=(0,o.k6)(),l=function(e){let{queryString:t=!1,groupId:a}=e;if("string"==typeof t)return t;if(!1===t)return null;if(!0===t&&!a)throw new Error('Docusaurus error: The <Tabs> component groupId prop is required if queryString=true, because this value is used as the search param name. You can also provide an explicit value such as queryString="my-search-param".');return a??null}({queryString:t,groupId:a});return[(0,s._X)(l),(0,r.useCallback)((e=>{if(!l)return;const t=new URLSearchParams(n.location.search);t.set(l,e),n.replace({...n.location,search:t.toString()})}),[l,n])]}function b(e){const{defaultValue:t,queryString:a=!1,groupId:n}=e,l=d(e),[i,o]=(0,r.useState)((()=>function(e){let{defaultValue:t,tabValues:a}=e;if(0===a.length)throw new Error("Docusaurus error: the <Tabs> component requires at least one <TabItem> children component");if(t){if(!m({value:t,tabValues:a}))throw new Error(`Docusaurus error: The <Tabs> has a defaultValue "${t}" but none of its children has the corresponding value. Available values are: ${a.map((e=>e.value)).join(", ")}. If you intend to show no default tab, use defaultValue={null} instead.`);return t}const n=a.find((e=>e.default))??a[0];if(!n)throw new Error("Unexpected error: 0 tabValues");return n.value}({defaultValue:t,tabValues:l}))),[s,u]=k({queryString:a,groupId:n}),[p,b]=function(e){let{groupId:t}=e;const a=function(e){return e?`docusaurus.tab.${e}`:null}(t),[n,l]=(0,c.Nk)(a);return[n,(0,r.useCallback)((e=>{a&&l.set(e)}),[a,l])]}({groupId:n}),f=(()=>{const e=s??p;return m({value:e,tabValues:l})?e:null})();(0,r.useLayoutEffect)((()=>{f&&o(f)}),[f]);return{selectedValue:i,selectValue:(0,r.useCallback)((e=>{if(!m({value:e,tabValues:l}))throw new Error(`Can't select invalid tab value=${e}`);o(e),u(e),b(e)}),[u,b,l]),tabValues:l}}var f=a(72389);const g={tabList:"tabList__CuJ",tabItem:"tabItem_LNqP"};function h(e){let{className:t,block:a,selectedValue:o,selectValue:s,tabValues:u}=e;const c=[],{blockElementScrollPositionUntilNextRender:p}=(0,i.o5)(),d=e=>{const t=e.currentTarget,a=c.indexOf(t),n=u[a].value;n!==o&&(p(t),s(n))},m=e=>{let t=null;switch(e.key){case"Enter":d(e);break;case"ArrowRight":{const a=c.indexOf(e.currentTarget)+1;t=c[a]??c[0];break}case"ArrowLeft":{const a=c.indexOf(e.currentTarget)-1;t=c[a]??c[c.length-1];break}}t?.focus()};return r.createElement("ul",{role:"tablist","aria-orientation":"horizontal",className:(0,l.Z)("tabs",{"tabs--block":a},t)},u.map((e=>{let{value:t,label:a,attributes:i}=e;return r.createElement("li",(0,n.Z)({role:"tab",tabIndex:o===t?0:-1,"aria-selected":o===t,key:t,ref:e=>c.push(e),onKeyDown:m,onClick:d},i,{className:(0,l.Z)("tabs__item",g.tabItem,i?.className,{"tabs__item--active":o===t})}),a??t)})))}function v(e){let{lazy:t,children:a,selectedValue:n}=e;const l=(Array.isArray(a)?a:[a]).filter(Boolean);if(t){const e=l.find((e=>e.props.value===n));return e?(0,r.cloneElement)(e,{className:"margin-top--md"}):null}return r.createElement("div",{className:"margin-top--md"},l.map(((e,t)=>(0,r.cloneElement)(e,{key:t,hidden:e.props.value!==n}))))}function y(e){const t=b(e);return r.createElement("div",{className:(0,l.Z)("tabs-container",g.tabList)},r.createElement(h,(0,n.Z)({},e,t)),r.createElement(v,(0,n.Z)({},e,t)))}function N(e){const t=(0,f.Z)();return r.createElement(y,(0,n.Z)({key:String(t)},e))}},16893:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>c,contentTitle:()=>s,default:()=>k,frontMatter:()=>o,metadata:()=>u,toc:()=>p});var n=a(87462),r=(a(67294),a(3905)),l=a(74866),i=a(85162);const o={},s="Databricks on any cloud",u={unversionedId:"platforms/databricks",id:"platforms/databricks",title:"Databricks on any cloud",description:"Cluster Setup",source:"@site/docs/0700-platforms/030.databricks.mdx",sourceDirName:"0700-platforms",slug:"/platforms/databricks",permalink:"/starlake/docs/next/platforms/databricks",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/docs/0700-platforms/030.databricks.mdx",tags:[],version:"current",sidebarPosition:30,frontMatter:{},sidebar:"starlakeSidebar",previous:{title:"Azure Synapse Spark Pools",permalink:"/starlake/docs/next/platforms/azure"},next:{title:"Google Cloud",permalink:"/starlake/docs/next/platforms/gcp"}},c={},p=[{value:"Cluster Setup",id:"cluster-setup",level:2},{value:"Create a service account",id:"create-a-service-account",level:2},{value:"Create a Databricks Cluster",id:"create-a-databricks-cluster",level:2},{value:"Environment Variables",id:"environment-variables",level:2},{value:"Mount DBFS",id:"mount-dbfs",level:2},{value:"Create a Starlake job",id:"create-a-starlake-job",level:2},{value:"Ingest your data",id:"ingest-your-data",level:2}],d={toc:p},m="wrapper";function k(e){let{components:t,...o}=e;return(0,r.kt)(m,(0,n.Z)({},d,o,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"databricks-on-any-cloud"},"Databricks on any cloud"),(0,r.kt)("h2",{id:"cluster-setup"},"Cluster Setup"),(0,r.kt)("p",null,"When running on top of Databricks, you can make complete abstraction of the Cloud provider.\nYou juste need to setup a Databricks cluster that will make use of the compute and storage resources provided by\nthe underlying cloud provider. Follow the steps below to run starlake on top of Databricks:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Create a service account"),(0,r.kt)("li",{parentName:"ul"},"Create a Databricks cluster"),(0,r.kt)("li",{parentName:"ul"},"Mount the Databricks File System"),(0,r.kt)("li",{parentName:"ul"},"Create a Starlake job"),(0,r.kt)("li",{parentName:"ul"},"Ingest your data")),(0,r.kt)("admonition",{type:"note"},(0,r.kt)("p",{parentName:"admonition"},"The screenshots below are taken from a Databricks cluster running on Google Cloud but are also valid for Azure and AWS.")),(0,r.kt)("h2",{id:"create-a-service-account"},"Create a service account"),(0,r.kt)(l.Z,{groupId:"environments",mdxType:"Tabs"},(0,r.kt)(i.Z,{value:"azure",label:"Azure",mdxType:"TabItem"},(0,r.kt)("p",null,"Create a storage account and name it for example ",(0,r.kt)("inlineCode",{parentName:"p"},"starlakestorage")," and assigns it to a resource group. In this storage account create a container that you can name ",(0,r.kt)("inlineCode",{parentName:"p"},"starlake-app")," and set its public access level to ",(0,r.kt)("inlineCode",{parentName:"p"},"Container"),". This container will have the following purposes:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Store Starlake jars"),(0,r.kt)("li",{parentName:"ul"},"Store Starlake metadata "),(0,r.kt)("li",{parentName:"ul"},"Store parquet files after ingestion")),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"Azure create container",src:a(60947).Z,title:"Azure create container",width:"419",height:"396"})),(0,r.kt)("p",null,"But you can also distribute these tasks across several containers.")),(0,r.kt)(i.Z,{value:"gcp",label:"Google Cloud Platform",mdxType:"TabItem"},(0,r.kt)("p",null,"Create a bucket and name it for example ",(0,r.kt)("inlineCode",{parentName:"p"},"starlake-app"),". This bucket will have the following purposes:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Store Starlake jars"),(0,r.kt)("li",{parentName:"ul"},"Store Starlake metadata "),(0,r.kt)("li",{parentName:"ul"},"Store parquet files after ingestion")),(0,r.kt)("p",null,"Create a service account and assign it the Storage Admin role. "),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"Create Service Account",src:a(50663).Z,title:"create service account",width:"567",height:"657"})))),(0,r.kt)("h2",{id:"create-a-databricks-cluster"},"Create a Databricks Cluster"),(0,r.kt)("p",null,"In a Databricks Workspace, create a cluster with the correct Databricks Runtime version : 9.1 LTS (Apache Spark 3.1.2, Scala 2.12)."),(0,r.kt)(l.Z,{groupId:"environments",mdxType:"Tabs"},(0,r.kt)(i.Z,{value:"azure",label:"Azure",mdxType:"TabItem"},(0,r.kt)("p",null,"In this section you don't have to set service account variables, we will set it when mounting our container. Just copy your storage account access key we will use it later in a notebook."),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"Azure Key",src:a(85256).Z,title:"Azure access key",width:"1699",height:"759"}))),(0,r.kt)(i.Z,{value:"gcp",label:"Google Cloud Platform",mdxType:"TabItem"},(0,r.kt)("p",null,"Set the value of the ",(0,r.kt)("inlineCode",{parentName:"p"},"Service Account")," field name to the service account you just create in the step above.\n",(0,r.kt)("img",{alt:"Create Databricks Cluster",src:a(86486).Z,title:"Create Databricks cluster",width:"567",height:"734"})),(0,r.kt)("p",null,"In the ",(0,r.kt)("inlineCode",{parentName:"p"},"Advanced Settings / Spark Config"),"  page set the variables below:"),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"Spark config",src:a(86795).Z,title:"Spark config",width:"850",height:"137"})),(0,r.kt)("table",null,(0,r.kt)("thead",{parentName:"table"},(0,r.kt)("tr",{parentName:"thead"},(0,r.kt)("th",{parentName:"tr",align:null},"Spark Config entry"),(0,r.kt)("th",{parentName:"tr",align:null},"Value"),(0,r.kt)("th",{parentName:"tr",align:null},"Description"))),(0,r.kt)("tbody",{parentName:"table"},(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"spark.hadoop.google.cloud.auth.service.account.enable"),(0,r.kt)("td",{parentName:"tr",align:null},"true"),(0,r.kt)("td",{parentName:"tr",align:null},"Enable service account auth")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"spark.hadoop.fs.gs.auth.service.account.email"),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("a",{parentName:"td",href:"mailto:me@dummy.iam.gserviceaccount.com"},"me@dummy.iam.gserviceaccount.com")),(0,r.kt)("td",{parentName:"tr",align:null},"Service account name")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"spark.hadoop.fs.gs.project.id"),(0,r.kt)("td",{parentName:"tr",align:null},"my-project-id-123456"),(0,r.kt)("td",{parentName:"tr",align:null},"Project id")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"spark.hadoop.fs.gs.auth.service.account.private.key"),(0,r.kt)("td",{parentName:"tr",align:null},"-----BEGIN PRIVATE KEY----- YOUR PRIVATE KEY GOES HERE-----END PRIVATE KEY-----"),(0,r.kt)("td",{parentName:"tr",align:null},"Private key as defined in your JSON file by the attribute ",(0,r.kt)("inlineCode",{parentName:"td"},"private_key"))),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"spark.hadoop.fs.gs.auth.service.account.private.key.id"),(0,r.kt)("td",{parentName:"tr",align:null},"df728e47e5e6c14402fafe6d39a3b8792a9967c7"),(0,r.kt)("td",{parentName:"tr",align:null},"Private key as defined in your JSON file by the attribute ",(0,r.kt)("inlineCode",{parentName:"td"},"private_key_id"))))))),(0,r.kt)("h2",{id:"environment-variables"},"Environment Variables"),(0,r.kt)("p",null,"In the ",(0,r.kt)("inlineCode",{parentName:"p"},"Advanced Settings / Environment variables"),"  section set the variables below:"),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"Environnement variables",src:a(71169).Z,title:"Environnement variables",width:"408",height:"178"})),(0,r.kt)("table",null,(0,r.kt)("thead",{parentName:"table"},(0,r.kt)("tr",{parentName:"thead"},(0,r.kt)("th",{parentName:"tr",align:null},"Environnement variable"),(0,r.kt)("th",{parentName:"tr",align:null},"Value"),(0,r.kt)("th",{parentName:"tr",align:null},"Description"))),(0,r.kt)("tbody",{parentName:"table"},(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"SL_METRICS_ACTIVE"),(0,r.kt)("td",{parentName:"tr",align:null},"true"),(0,r.kt)("td",{parentName:"tr",align:null},"Should we compute metrics set on individuals table columns at ingestion time")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"SL_ROOT"),(0,r.kt)("td",{parentName:"tr",align:null},"/mnt/starlake-app/tmp/userguide"),(0,r.kt)("td",{parentName:"tr",align:null},"This is a DBFS mounted directory (see below). It should reference the base directory where your starlake metadata is located")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"SL_AUDIT_SINK_TYPE"),(0,r.kt)("td",{parentName:"tr",align:null},"BigQuerySink"),(0,r.kt)("td",{parentName:"tr",align:null},"Where to save audit logs. Here we decide to save it in BigQuery. To have it as a hive table or file on the cloud storage, set it to FsSink")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"SL_FS"),(0,r.kt)("td",{parentName:"tr",align:null},"dbfs://"),(0,r.kt)("td",{parentName:"tr",align:null},"Filesystem. Always set it to DBFS in Databricks.")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"SL_HIVE"),(0,r.kt)("td",{parentName:"tr",align:null},"true"),(0,r.kt)("td",{parentName:"tr",align:null},"Should we store the resulting parquet files as Databricks tables ?")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"TEMPORARY_GCS_BUCKET"),(0,r.kt)("td",{parentName:"tr",align:null},"starlake-app"),(0,r.kt)("td",{parentName:"tr",align:null},"Bucket name where Google Cloud API store temporary files when saving data to BigQuery. Don't add this one if you're on Azure")))),(0,r.kt)("h2",{id:"mount-dbfs"},"Mount DBFS"),(0,r.kt)("p",null,"Databricks virtualize the underlying filesystem through DBFS. We first need to enable it in ",(0,r.kt)("inlineCode",{parentName:"p"},"Admin Console / Workspace Settings")," page:"),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"Enable DBFS",src:a(5823).Z,title:"Enable DBFS",width:"850",height:"478"})),(0,r.kt)("p",null,"We now inside a ",(0,r.kt)("inlineCode",{parentName:"p"},"notebook")," mount the cloud storage bucket created above and referenced in the cluster environment variables into DBFS:"),(0,r.kt)(l.Z,{groupId:"environments",mdxType:"Tabs"},(0,r.kt)(i.Z,{value:"azure",label:"Azure",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-python"},'storage_name= "starlakestorage"\ncontainer_name = "starlake-app"\nstorage_acces_key = "Your access key"\nmount_name = "starlake-app"\ndbutils.fs.mount(\n  source="wasbs://%s@%s.blob.core.windows.net" % (container_name, storage_name),\n  mount_point="/mnt/%s" % mount_name,\n  extra_configs = {\n    "fs.azure.account.key.%s.blob.core.windows.net" % storage_name: storage_acces_key\n  }  \n)\n'))),(0,r.kt)(i.Z,{value:"gcp",label:"Google Cloud Platform",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-python"},'bucket_name = "starlake-app"\nmount_name = "starlake-app"\ndbutils.fs.mount("gs://%s" % bucket_name, "/mnt/%s" % mount_name)\ndisplay(dbutils.fs.ls("/mnt/%s" % mount_name))\n')))),(0,r.kt)("p",null,"Your storage account is now accessible on Databricks as a folder from Spark as the folder ",(0,r.kt)("inlineCode",{parentName:"p"},"dbfs:/mnt/starlake-app")),(0,r.kt)("h2",{id:"create-a-starlake-job"},"Create a Starlake job"),(0,r.kt)("p",null,"To create a starlake job, you first upload the starlake uber jar and the jackson yaml (de)serializer into the gs://starlake-app folder or starlake-app container"),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"conatiner details Azure",src:a(87173).Z,title:"container details Azure",width:"954",height:"444"})),(0,r.kt)("p",null,"The version of the ",(0,r.kt)("inlineCode",{parentName:"p"},"jackson-dataformat-yaml")," depends follows the version\nof the others ",(0,r.kt)("inlineCode",{parentName:"p"},"jackson")," components referenced by the databricks runtime. Download the correct version on ",(0,r.kt)("a",{parentName:"p",href:"https://central.sonatype.com/artifact/com.fasterxml.jackson.dataformat/jackson-dataformat-yaml/2.12.3/versions"},"Maven Central"),". Then add the starlake assembly jar that you can find ",(0,r.kt)("a",{parentName:"p",href:"https://central.sonatype.com/artifact/ai.starlake/starlake-spark3_2.12/0.7.4/versions"},"here"),"."),(0,r.kt)("p",null,"Note that only ",(0,r.kt)("inlineCode",{parentName:"p"},"import"),", ",(0,r.kt)("inlineCode",{parentName:"p"},"watch/load/ingest"),", ",(0,r.kt)("inlineCode",{parentName:"p"},"transform")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"metrics")," command lines are designed exclusively for production environments"),(0,r.kt)("p",null,"You will need first to write your metadata configuration files from a local environment and then upload your starlake project in the ",(0,r.kt)("inlineCode",{parentName:"p"},"SL_ROOT")," location like this :"),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"metadata container",src:a(63497).Z,title:"metadata container",width:"951",height:"481"})),(0,r.kt)("p",null,"If you are working with an external data source, you can mount your data incoming location in databricks and reference it in the ",(0,r.kt)("inlineCode",{parentName:"p"},"env.sl.yml")," file:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="env.sl.yml"',title:'"env.sl.yml"'},'env:\n  root_path: "/mnt/starlake/tmp/userguide"\n  incoming_path: "/mnt/sample-data"\n')),(0,r.kt)("p",null,"Then reference your data source by using ",(0,r.kt)("inlineCode",{parentName:"p"},"{{incoming_path}}")," variable in the domains schemas"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="hr.sl.yml"',title:'"hr.sl.yml"'},'load:\n  name: "HR"\n  metadata:\n    mode: "FILE"\n    format: "JSON"\n    encoding: "UTF-8"\n    multiline: false\n    array: true\n    separator: "["\n    quote: "\\""\n    escape: "\\\\"\n    write: "APPEND"\n    directory: "{{incoming_path}}/HR"\n')),(0,r.kt)("p",null,"Create tasks and reference the two jars you uploaded and are now visible to databricks through the ",(0,r.kt)("inlineCode",{parentName:"p"},"dbfs:/mnt/starlake-app")," mount"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"The first task (",(0,r.kt)("inlineCode",{parentName:"li"},"import"),") will copy the files matching the expected patterns into the pending directory for ingestion by starlake ")),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"starlake import",src:a(23452).Z,title:"starlake import",width:"567",height:"589"})),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"The second task (",(0,r.kt)("inlineCode",{parentName:"li"},"watch"),") will run the starlake ingestion job store the result as parquet files in your dataset directory")),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"starlake watch",src:a(60138).Z,title:"starlake watch",width:"567",height:"581"})),(0,r.kt)("h2",{id:"ingest-your-data"},"Ingest your data"),(0,r.kt)("p",null,"Start the ",(0,r.kt)("inlineCode",{parentName:"p"},"import")," task first and then the ",(0,r.kt)("inlineCode",{parentName:"p"},"watch")," task. The execution logs are available through the ",(0,r.kt)("inlineCode",{parentName:"p"},"runs")," tab:"),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"tasks runs",src:a(85945).Z,title:"tasks runs",width:"850",height:"160"})),(0,r.kt)("p",null,"Since we set the ",(0,r.kt)("inlineCode",{parentName:"p"},"SL_HIVE=true")," environnment variable, ingested data are also available as tables"),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"starlake watch",src:a(45338).Z,title:"starlake watch",width:"489",height:"506"})),(0,r.kt)("p",null,"The audit log for the above tasks will be stored in a BigQuery table if we set ",(0,r.kt)("inlineCode",{parentName:"p"},"SL_AUDIT_SINK_TYPE=BigQuerySink")," environnment variable"))}k.isMDXComponent=!0},5823:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/advanced-settings-557c460604b44e4ada3add8f3518fd2c.png"},60947:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/azure-container-a18e69df9363abb80ec52955a9c570af.png"},85256:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/azure-key-e9847c37fa81bd1bb9559f9acdf557d5.png"},86486:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/cluster-061e61907e2729c200ceb348c4396400.png"},50663:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/create-service-account-ddf85b90ed3c391578883bee5dc8dc51.png"},45338:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/database-6f41bf4bd78f1759a22d9391da669cc8.png"},71169:(e,t,a)=>{a.d(t,{Z:()=>n});const n="data:image/png;base64,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"},23452:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/import-task-307566eb7703fe91e948db194c66ba24.png"},87173:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/jars-azure-5503a7cbecc74c45ed84051582f24c92.png"},63497:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/metadata-container-d91934cfa3a8cf8aa18f9c85d6b7109e.png"},85945:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/runs-d5061c6815ef1bd09b303a0f34c9aeb6.png"},86795:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/spark-config-fd932bc9ba618ef7f0066196861acb1a.png"},60138:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/watch-task-c95dc78dd2cc801ed24340f64c3aa7be.png"}}]);