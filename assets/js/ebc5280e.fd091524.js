"use strict";(self.webpackChunkstarlake_docs=self.webpackChunkstarlake_docs||[]).push([[2024],{3905:(e,t,a)=>{a.d(t,{Zo:()=>c,kt:()=>g});var r=a(67294);function n(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function o(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,r)}return a}function l(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?o(Object(a),!0).forEach((function(t){n(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):o(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function i(e,t){if(null==e)return{};var a,r,n=function(e,t){if(null==e)return{};var a,r,n={},o=Object.keys(e);for(r=0;r<o.length;r++)a=o[r],t.indexOf(a)>=0||(n[a]=e[a]);return n}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(r=0;r<o.length;r++)a=o[r],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(n[a]=e[a])}return n}var s=r.createContext({}),p=function(e){var t=r.useContext(s),a=t;return e&&(a="function"==typeof e?e(t):l(l({},t),e)),a},c=function(e){var t=p(e.components);return r.createElement(s.Provider,{value:t},e.children)},u="mdxType",d={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},m=r.forwardRef((function(e,t){var a=e.components,n=e.mdxType,o=e.originalType,s=e.parentName,c=i(e,["components","mdxType","originalType","parentName"]),u=p(a),m=n,g=u["".concat(s,".").concat(m)]||u[m]||d[m]||o;return a?r.createElement(g,l(l({ref:t},c),{},{components:a})):r.createElement(g,l({ref:t},c))}));function g(e,t){var a=arguments,n=t&&t.mdxType;if("string"==typeof e||n){var o=a.length,l=new Array(o);l[0]=m;var i={};for(var s in t)hasOwnProperty.call(t,s)&&(i[s]=t[s]);i.originalType=e,i[u]="string"==typeof e?e:n,l[1]=i;for(var p=2;p<o;p++)l[p]=a[p];return r.createElement.apply(null,l)}return r.createElement.apply(null,a)}m.displayName="MDXCreateElement"},50317:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>s,contentTitle:()=>l,default:()=>d,frontMatter:()=>o,metadata:()=>i,toc:()=>p});var r=a(87462),n=(a(67294),a(3905));const o={},l="Google Cloud Dataproc",i={unversionedId:"platform/gcp",id:"platform/gcp",title:"Google Cloud Dataproc",description:"Two options are available when running on Google Cloud:",source:"@site/docs/070-platform/040.gcp.md",sourceDirName:"070-platform",slug:"/platform/gcp",permalink:"/starlake/docs/next/platform/gcp",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/docs/070-platform/040.gcp.md",tags:[],version:"current",sidebarPosition:40,frontMatter:{},sidebar:"starlakeSidebar",previous:{title:"Databricks on any cloud",permalink:"/starlake/docs/next/platform/databricks"},next:{title:"Local filesystem",permalink:"/starlake/docs/next/platform/file"}},s={},p=[{value:"Production Mode with Google Dataproc",id:"production-mode-with-google-dataproc",level:2},{value:"Create a service account",id:"create-a-service-account",level:3},{value:"Create a Dataproc cluster",id:"create-a-dataproc-cluster",level:3},{value:"Create a Starlake job",id:"create-a-starlake-job",level:3},{value:"Ingest your data",id:"ingest-your-data",level:3},{value:"Running Locally with Spark (Dev. Mode)",id:"running-locally-with-spark-dev-mode",level:2}],c={toc:p},u="wrapper";function d(e){let{components:t,...o}=e;return(0,n.kt)(u,(0,r.Z)({},c,o,{components:t,mdxType:"MDXLayout"}),(0,n.kt)("h1",{id:"google-cloud-dataproc"},"Google Cloud Dataproc"),(0,n.kt)("p",null,"Two options are available when running on Google Cloud:"),(0,n.kt)("ul",null,(0,n.kt)("li",{parentName:"ul"},"Production Mode: Running the jobs against BigQuery with the Dataproc cluster executing on Google Cloud"),(0,n.kt)("li",{parentName:"ul"},"Development Mode: Running the jobs against BigQuery with a Spark Job running on your computer.")),(0,n.kt)("p",null,"In both cases, the metadata are stored in Google CLoud Storage, only the spark job is either running\non Dataproc or locally on your computer"),(0,n.kt)("h2",{id:"production-mode-with-google-dataproc"},"Production Mode with Google Dataproc"),(0,n.kt)("p",null,"Follow the steps below to run starlake on Google Cloud:"),(0,n.kt)("ul",null,(0,n.kt)("li",{parentName:"ul"},"Create a service account"),(0,n.kt)("li",{parentName:"ul"},"Create a Dataproc cluster"),(0,n.kt)("li",{parentName:"ul"},"Create a Starlake job"),(0,n.kt)("li",{parentName:"ul"},"Ingest your data")),(0,n.kt)("h3",{id:"create-a-service-account"},"Create a service account"),(0,n.kt)("p",null,"Create a bucket and name it for example ",(0,n.kt)("inlineCode",{parentName:"p"},"starlake-app"),". This bucket will have the following purposes:"),(0,n.kt)("ul",null,(0,n.kt)("li",{parentName:"ul"},"Store Starlake jars"),(0,n.kt)("li",{parentName:"ul"},"Store Starlake metadata"),(0,n.kt)("li",{parentName:"ul"},"Crteate datasets and tables in BigQuery. since we are ingesting into BigQuery, no parquet file is store on Cloud Storage.")),(0,n.kt)("p",null,"Create a service account and assign it the Storage Admin and BigQuery Admin roles. Depending on your security configuration,\nyou may be required to use lower access rights.  "),(0,n.kt)("p",null,(0,n.kt)("img",{alt:"Create Service Account",src:a(96011).Z,title:"create service account",width:"850",height:"627"})),(0,n.kt)("p",null,"Copy your starlake project to the ",(0,n.kt)("inlineCode",{parentName:"p"},"gd://starlake-app/mnt/quickstart")," directory using the following script run from\nthe ",(0,n.kt)("a",{parentName:"p",href:"https://github.com/starlake-ai/starlake/tree/master/samples/cloud"},"samples/cloud")," folder:"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-bash"},"\ngsutil cp -r quickstart/ gs://staralke-app/mnt/starlake-app/\n\n")),(0,n.kt)("h3",{id:"create-a-dataproc-cluster"},"Create a Dataproc cluster"),(0,n.kt)("p",null,"Dataproc is the Google service for running Spark jobs. After enabling the Dataproc API, create a cluster and define\nthe environnment variables below:"),(0,n.kt)("p",null,(0,n.kt)("img",{alt:"Create Dataproc cluster",src:a(84592).Z,title:"Create Dataproc cluster",width:"850",height:"579"})),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:null},"Environnement variable"),(0,n.kt)("th",{parentName:"tr",align:null},"Value"),(0,n.kt)("th",{parentName:"tr",align:null},"Description"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},"SL_ROOT"),(0,n.kt)("td",{parentName:"tr",align:null},"/mnt/starlake-app"),(0,n.kt)("td",{parentName:"tr",align:null},"It should reference the base directory where your starlake metadata is located")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},"SL_AUDIT_SINK_TYPE"),(0,n.kt)("td",{parentName:"tr",align:null},"BigQuerySink"),(0,n.kt)("td",{parentName:"tr",align:null},"Where to save audit logs. Here we decide to save it in BigQuery. Tos ave it as a hive table or file on the cloud storage, set it to FsSink")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},"SL_FS"),(0,n.kt)("td",{parentName:"tr",align:null},"gs://starlake-app"),(0,n.kt)("td",{parentName:"tr",align:null},"Filesystem. Reference the cloud storage bucket where all the files will be located.")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},"TEMPORARY_GCS_BUCKET"),(0,n.kt)("td",{parentName:"tr",align:null},"starlake-app"),(0,n.kt)("td",{parentName:"tr",align:null},"Bucket name where Google Cloud API store temporary files when saving data to BigQuery")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},"SL_ENV"),(0,n.kt)("td",{parentName:"tr",align:null},"BQ"),(0,n.kt)("td",{parentName:"tr",align:null},"Starlake Env variables. This will instruct Starlake to use the env.",(0,n.kt)("inlineCode",{parentName:"td"},"BQ"),".sl.yml file located at the root of your project when running comet. The ",(0,n.kt)("inlineCode",{parentName:"td"},"sink_type")," in this file instruct Starlake to save datasets in BigQuery instead of parquet files in Cloud Storage.")))),(0,n.kt)("p",null,"To create the dataproc cluster using the CLI instead, just run the command below:"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-bash"},"\ngcloud dataproc clusters create cluster-88ea \\\n      --region europe-west1 \\\n      --zone europe-west1-b \\\n      --master-machine-type n1-standard-4 \\\n      --master-boot-disk-size 500 \\\n      --num-workers 2 \\\n      --worker-machine-type n1-standard-4 \\\n      --worker-boot-disk-size 500 \\\n      --image-version 2.0-debian10 \\\n      --project my-starlake-project-id \\\n      --properties \\\n        spark-env:SL_AUDIT_SINK_TYPE=BigQuerySink, \\\n        spark-env:SL_ENV=BQ, \\\n        spark-env:SL_FS=gs://starlake-app, \\\n        spark-env:SL_ROOT=/mnt/quickstart, \\\n        spark-env:TEMPORARY_GCS_BUCKET=starlake-app\n\n")),(0,n.kt)("h3",{id:"create-a-starlake-job"},"Create a Starlake job"),(0,n.kt)("p",null,"Assuming that you copied the starlake assembly to the root for the gs://starlake-app bucket,\njust create using the POST request below or through the user interface."),(0,n.kt)("p",null,(0,n.kt)("img",{alt:"Create Dataproc job",src:a(77954).Z,title:"Create Dataproc job",width:"850",height:"1205"})),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-bash"},'\nPOST /v1/projects/my-starlake-project-id/regions/europe-west1/jobs:submit/\n{\n  "projectId": "my-starlake-project",\n  "job": {\n    "placement": {},\n    "statusHistory": [],\n    "reference": {\n      "jobId": "job-aacf2cd5",\n      "projectId": "my-starlake-project-id"\n    },\n    "sparkJob": {\n      "mainClass": "ai.starlake.job.Main",\n      "properties": {},\n      "jarFileUris": [\n        "gs://starlake-app/starlake-VERSION-assembly.jar",\n        "gs://spark-lib/bigquery/spark-bigquery-latest_2.12.jar"\n      ],\n      "args": [\n        "import"\n      ]\n    }\n  }\n}\n\n')),(0,n.kt)("p",null,"We create above the import job. To create the watch job, just create a new job and replace ",(0,n.kt)("inlineCode",{parentName:"p"},"import"),"by ",(0,n.kt)("inlineCode",{parentName:"p"},"watch")," in\nthe ",(0,n.kt)("inlineCode",{parentName:"p"},"Arguments")," field as shown below:"),(0,n.kt)("p",null,(0,n.kt)("img",{alt:"Watch job",src:a(74947).Z,title:"Watch job",width:"850",height:"122"})),(0,n.kt)("h3",{id:"ingest-your-data"},"Ingest your data"),(0,n.kt)("p",null,"Start the ",(0,n.kt)("inlineCode",{parentName:"p"},"import")," job first and then the ",(0,n.kt)("inlineCode",{parentName:"p"},"watch")," job. The execution logs are available in the Dataproc UI:"),(0,n.kt)("p",null,(0,n.kt)("img",{alt:"tasks runs",src:a(2212).Z,title:"tasks runs",width:"850",height:"98"})),(0,n.kt)("p",null,"Since we ingested data into BigQuery, We find it available in BigQuery datasets and tables\n",(0,n.kt)("img",{alt:"starlake watch",src:a(71948).Z,title:"starlake watch",width:"1560",height:"1064"})),(0,n.kt)("p",null,"The audit log for the above jobs are available in a BigQuery table since we set the ",(0,n.kt)("inlineCode",{parentName:"p"},"SL_AUDIT_SINK_TYPE=BigQuerySink")," environnment variable."),(0,n.kt)("h2",{id:"running-locally-with-spark-dev-mode"},"Running Locally with Spark (Dev. Mode)"),(0,n.kt)("p",null,"When describing your data format, you may need to run, for testing purposes, your job locally against the remote GCP Project hosting your BigQuery datasets.\nIn that case, you need to set the GCP_PROJECT env var and create a custom core-site.xml in your classpath as described below :"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-xml"}," <configuration>\n     <property>\n         <name>fs.gs.impl</name>\n         <value>com.google.cloud.hadoop.fs.gcs.GoogleHadoopFileSystem</value>\n     </property>\n     <property>\n         <name>fs.AbstractFileSystem.gs.impl</name>\n         <value>com.google.cloud.hadoop.fs.gcs.GoogleHadoopFS</value>\n     </property>\n     <property>\n         <name>fs.gs.project.id</name>\n         <value>myproject-1234</value>\n     </property>\n     <property>\n         <name>google.cloud.auth.service.account.enable</name>\n         <value>true</value>\n     </property>\n     <property>\n         <name>google.cloud.auth.service.account.json.keyfile</name>\n         <value>/Users/me/.gcloud/keys/myproject-1234.json</value>\n     </property>\n     <property>\n         <name>fs.default.name</name>\n         <value>gs://startlake-app</value>\n     </property>\n     <property>\n         <name>fs.defaultFS</name>\n         <value>gs://startlake-app</value>\n     </property>\n     <property>\n         <name>fs.gs.system.bucket</name>\n         <value>startlake-app</value>\n     </property>\n </configuration>\n")))}d.isMDXComponent=!0},71948:(e,t,a)=>{a.d(t,{Z:()=>r});const r=a.p+"assets/images/bigquery-82292e4b7de01626a23fdf348c3d2c85.png"},84592:(e,t,a)=>{a.d(t,{Z:()=>r});const r=a.p+"assets/images/create-dataproc-015c4fffce23828183a0e5554ea9a983.png"},77954:(e,t,a)=>{a.d(t,{Z:()=>r});const r=a.p+"assets/images/create-import-job-970b379ee437c890078d3b20a31e6eb1.png"},96011:(e,t,a)=>{a.d(t,{Z:()=>r});const r=a.p+"assets/images/create-service-account-5645ba303a6b049b7de97d9c236eb3ce.png"},74947:(e,t,a)=>{a.d(t,{Z:()=>r});const r=a.p+"assets/images/create-watch-job-0dccfab45b80fa6f1afaef00957e178c.png"},2212:(e,t,a)=>{a.d(t,{Z:()=>r});const r=a.p+"assets/images/runs-f87c0dd6d17d6a0fde9aaefe91cc98a6.png"}}]);