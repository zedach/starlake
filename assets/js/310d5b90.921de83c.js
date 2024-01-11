"use strict";(self.webpackChunkstarlake=self.webpackChunkstarlake||[]).push([[1441],{3905:(e,t,a)=>{a.d(t,{Zo:()=>d,kt:()=>g});var n=a(67294);function r(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function i(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function o(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?i(Object(a),!0).forEach((function(t){r(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):i(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function l(e,t){if(null==e)return{};var a,n,r=function(e,t){if(null==e)return{};var a,n,r={},i=Object.keys(e);for(n=0;n<i.length;n++)a=i[n],t.indexOf(a)>=0||(r[a]=e[a]);return r}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(n=0;n<i.length;n++)a=i[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(r[a]=e[a])}return r}var s=n.createContext({}),p=function(e){var t=n.useContext(s),a=t;return e&&(a="function"==typeof e?e(t):o(o({},t),e)),a},d=function(e){var t=p(e.components);return n.createElement(s.Provider,{value:t},e.children)},c="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},m=n.forwardRef((function(e,t){var a=e.components,r=e.mdxType,i=e.originalType,s=e.parentName,d=l(e,["components","mdxType","originalType","parentName"]),c=p(a),m=r,g=c["".concat(s,".").concat(m)]||c[m]||u[m]||i;return a?n.createElement(g,o(o({ref:t},d),{},{components:a})):n.createElement(g,o({ref:t},d))}));function g(e,t){var a=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var i=a.length,o=new Array(i);o[0]=m;var l={};for(var s in t)hasOwnProperty.call(t,s)&&(l[s]=t[s]);l.originalType=e,l[c]="string"==typeof e?e:r,o[1]=l;for(var p=2;p<i;p++)o[p]=a[p];return n.createElement.apply(null,o)}return n.createElement.apply(null,a)}m.displayName="MDXCreateElement"},85162:(e,t,a)=>{a.d(t,{Z:()=>o});var n=a(67294),r=a(86010);const i={tabItem:"tabItem_Ymn6"};function o(e){let{children:t,hidden:a,className:o}=e;return n.createElement("div",{role:"tabpanel",className:(0,r.Z)(i.tabItem,o),hidden:a},t)}},74866:(e,t,a)=>{a.d(t,{Z:()=>w});var n=a(87462),r=a(67294),i=a(86010),o=a(12466),l=a(16550),s=a(91980),p=a(67392),d=a(50012);function c(e){return function(e){return r.Children.map(e,(e=>{if(!e||(0,r.isValidElement)(e)&&function(e){const{props:t}=e;return!!t&&"object"==typeof t&&"value"in t}(e))return e;throw new Error(`Docusaurus error: Bad <Tabs> child <${"string"==typeof e.type?e.type:e.type.name}>: all children of the <Tabs> component should be <TabItem>, and every <TabItem> should have a unique "value" prop.`)}))?.filter(Boolean)??[]}(e).map((e=>{let{props:{value:t,label:a,attributes:n,default:r}}=e;return{value:t,label:a,attributes:n,default:r}}))}function u(e){const{values:t,children:a}=e;return(0,r.useMemo)((()=>{const e=t??c(a);return function(e){const t=(0,p.l)(e,((e,t)=>e.value===t.value));if(t.length>0)throw new Error(`Docusaurus error: Duplicate values "${t.map((e=>e.value)).join(", ")}" found in <Tabs>. Every value needs to be unique.`)}(e),e}),[t,a])}function m(e){let{value:t,tabValues:a}=e;return a.some((e=>e.value===t))}function g(e){let{queryString:t=!1,groupId:a}=e;const n=(0,l.k6)(),i=function(e){let{queryString:t=!1,groupId:a}=e;if("string"==typeof t)return t;if(!1===t)return null;if(!0===t&&!a)throw new Error('Docusaurus error: The <Tabs> component groupId prop is required if queryString=true, because this value is used as the search param name. You can also provide an explicit value such as queryString="my-search-param".');return a??null}({queryString:t,groupId:a});return[(0,s._X)(i),(0,r.useCallback)((e=>{if(!i)return;const t=new URLSearchParams(n.location.search);t.set(i,e),n.replace({...n.location,search:t.toString()})}),[i,n])]}function h(e){const{defaultValue:t,queryString:a=!1,groupId:n}=e,i=u(e),[o,l]=(0,r.useState)((()=>function(e){let{defaultValue:t,tabValues:a}=e;if(0===a.length)throw new Error("Docusaurus error: the <Tabs> component requires at least one <TabItem> children component");if(t){if(!m({value:t,tabValues:a}))throw new Error(`Docusaurus error: The <Tabs> has a defaultValue "${t}" but none of its children has the corresponding value. Available values are: ${a.map((e=>e.value)).join(", ")}. If you intend to show no default tab, use defaultValue={null} instead.`);return t}const n=a.find((e=>e.default))??a[0];if(!n)throw new Error("Unexpected error: 0 tabValues");return n.value}({defaultValue:t,tabValues:i}))),[s,p]=g({queryString:a,groupId:n}),[c,h]=function(e){let{groupId:t}=e;const a=function(e){return e?`docusaurus.tab.${e}`:null}(t),[n,i]=(0,d.Nk)(a);return[n,(0,r.useCallback)((e=>{a&&i.set(e)}),[a,i])]}({groupId:n}),b=(()=>{const e=s??c;return m({value:e,tabValues:i})?e:null})();(0,r.useLayoutEffect)((()=>{b&&l(b)}),[b]);return{selectedValue:o,selectValue:(0,r.useCallback)((e=>{if(!m({value:e,tabValues:i}))throw new Error(`Can't select invalid tab value=${e}`);l(e),p(e),h(e)}),[p,h,i]),tabValues:i}}var b=a(72389);const k={tabList:"tabList__CuJ",tabItem:"tabItem_LNqP"};function y(e){let{className:t,block:a,selectedValue:l,selectValue:s,tabValues:p}=e;const d=[],{blockElementScrollPositionUntilNextRender:c}=(0,o.o5)(),u=e=>{const t=e.currentTarget,a=d.indexOf(t),n=p[a].value;n!==l&&(c(t),s(n))},m=e=>{let t=null;switch(e.key){case"Enter":u(e);break;case"ArrowRight":{const a=d.indexOf(e.currentTarget)+1;t=d[a]??d[0];break}case"ArrowLeft":{const a=d.indexOf(e.currentTarget)-1;t=d[a]??d[d.length-1];break}}t?.focus()};return r.createElement("ul",{role:"tablist","aria-orientation":"horizontal",className:(0,i.Z)("tabs",{"tabs--block":a},t)},p.map((e=>{let{value:t,label:a,attributes:o}=e;return r.createElement("li",(0,n.Z)({role:"tab",tabIndex:l===t?0:-1,"aria-selected":l===t,key:t,ref:e=>d.push(e),onKeyDown:m,onClick:u},o,{className:(0,i.Z)("tabs__item",k.tabItem,o?.className,{"tabs__item--active":l===t})}),a??t)})))}function f(e){let{lazy:t,children:a,selectedValue:n}=e;const i=(Array.isArray(a)?a:[a]).filter(Boolean);if(t){const e=i.find((e=>e.props.value===n));return e?(0,r.cloneElement)(e,{className:"margin-top--md"}):null}return r.createElement("div",{className:"margin-top--md"},i.map(((e,t)=>(0,r.cloneElement)(e,{key:t,hidden:e.props.value!==n}))))}function v(e){const t=h(e);return r.createElement("div",{className:(0,i.Z)("tabs-container",k.tabList)},r.createElement(y,(0,n.Z)({},e,t)),r.createElement(f,(0,n.Z)({},e,t)))}function w(e){const t=(0,b.Z)();return r.createElement(v,(0,n.Z)({key:String(t)},e))}},1140:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>d,contentTitle:()=>s,default:()=>g,frontMatter:()=>l,metadata:()=>p,toc:()=>c});var n=a(87462),r=(a(67294),a(3905)),i=a(74866),o=a(85162);const l={slug:"spark-big-query-partitioning",title:"Handling Dynamic Partitioning and Merge with Spark on BigQuery",author:"Hayssam Saleh",author_title:"Starlake Core Team Member",author_url:"https://www.linkedin.com/in/hayssams/",author_image_url:"https://s.gravatar.com/avatar/04aa2a859a66b52787bcba8c36beba8c.png",tags:["Spark","BigQuery","Dataproc","Google Cloud","ETL","Starlake"]},s=void 0,p={permalink:"/starlake/blog/spark-big-query-partitioning",editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/blog/2021-12-15-spark-big-query-partitioning.mdx",source:"@site/blog/2021-12-15-spark-big-query-partitioning.mdx",title:"Handling Dynamic Partitioning and Merge with Spark on BigQuery",description:"Data Loading strategies",date:"2021-12-15T00:00:00.000Z",formattedDate:"December 15, 2021",tags:[{label:"Spark",permalink:"/starlake/blog/tags/spark"},{label:"BigQuery",permalink:"/starlake/blog/tags/big-query"},{label:"Dataproc",permalink:"/starlake/blog/tags/dataproc"},{label:"Google Cloud",permalink:"/starlake/blog/tags/google-cloud"},{label:"ETL",permalink:"/starlake/blog/tags/etl"},{label:"Starlake",permalink:"/starlake/blog/tags/starlake"}],readingTime:6.715,hasTruncateMarker:!1,authors:[{name:"Hayssam Saleh",title:"Starlake Core Team Member",url:"https://www.linkedin.com/in/hayssams/",imageURL:"https://s.gravatar.com/avatar/04aa2a859a66b52787bcba8c36beba8c.png"}],frontMatter:{slug:"spark-big-query-partitioning",title:"Handling Dynamic Partitioning and Merge with Spark on BigQuery",author:"Hayssam Saleh",author_title:"Starlake Core Team Member",author_url:"https://www.linkedin.com/in/hayssams/",author_image_url:"https://s.gravatar.com/avatar/04aa2a859a66b52787bcba8c36beba8c.png",tags:["Spark","BigQuery","Dataproc","Google Cloud","ETL","Starlake"]},prevItem:{title:"Column  and Row Level Security in BigQuery",permalink:"/starlake/blog/rls-cls-big-query"},nextItem:{title:"Bonjour",permalink:"/starlake/blog/bonjour"}},d={authorsImageUrls:[void 0]},c=[{value:"Data Loading strategies",id:"data-loading-strategies",level:2},{value:"Append and Overwrite modes in Spark",id:"append-and-overwrite-modes-in-spark",level:2},{value:"Dynamic Partition Overwrite mode in Spark",id:"dynamic-partition-overwrite-mode-in-spark",level:3},{value:"Dynamic Partition Merge in Spark",id:"dynamic-partition-merge-in-spark",level:3},{value:"Starlake How-to",id:"starlake-how-to",level:2},{value:"Comments",id:"comments",level:2}],u={toc:c},m="wrapper";function g(e){let{components:t,...l}=e;return(0,r.kt)(m,(0,n.Z)({},u,l,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h2",{id:"data-loading-strategies"},"Data Loading strategies"),(0,r.kt)("p",null,"When loading data into BigQuery, you may want to:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("strong",{parentName:"li"},"Overwrite")," the existing data and replace it with the incoming data."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("strong",{parentName:"li"},"Append")," incoming data to existing"),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("strong",{parentName:"li"},"Dynamic partition Overwrite")," where only the partitions to which the incoming data belong to are overwritten."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("strong",{parentName:"li"},"Merge")," incoming data with existing data by keeping the newest version of each record.")),(0,r.kt)("p",null,"For performance reasons, when having huge amount of data, tables are usually split\ninto multiple partitions. BigQuery supports range partitioning which are uncommon and date/time partitioning\nwhich is the most widely used type of partitioning.\nThe diagram below shows our initial table partitioned by the ",(0,r.kt)("inlineCode",{parentName:"p"},"date")," field."),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"Initial data",src:a(47808).Z,title:"Initial data",width:"740",height:"311"})),(0,r.kt)("p",null,"Let's assume we receive the following data that we need to ingest into the table:"),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"Incoming data",src:a(19893).Z,title:"Incoming data",width:"257",height:"78"})),(0,r.kt)("p",null,"The strategies above will produce respectively the results below:"),(0,r.kt)(i.Z,{groupId:"modes",mdxType:"Tabs"},(0,r.kt)(o.Z,{value:"overwrite",label:"Overwrite",mdxType:"TabItem"},(0,r.kt)("p",null,"The table ends up with the 2 incoming records.\nAll existing partitions are deleted."),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"Overwrite data",src:a(22502).Z,title:"Overwrite data",width:"724",height:"292"}))),(0,r.kt)(o.Z,{value:"append",label:"Append",mdxType:"TabItem"},(0,r.kt)("p",null,"The table ends up with 7 records. Note that a new ",(0,r.kt)("inlineCode",{parentName:"p"},"\xectem 1")," record is added while the older one is kept."),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"Append data",src:a(33112).Z,title:"Append data",width:"718",height:"358"}))),(0,r.kt)(o.Z,{value:"partitioned-overwrite",label:"Dynamic partition Overwrite",mdxType:"TabItem"},(0,r.kt)("p",null,"The table ends up with 4 records.\nThe second partition remains untouched while the first partition is erased and overwritten by with the incoming data."),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"Merge data",src:a(95517).Z,title:"Merge data",width:"724",height:"268"}))),(0,r.kt)(o.Z,{value:"merge",label:"Merge",mdxType:"TabItem"},(0,r.kt)("p",null,"The table ends up with 4 records.\nIncoming and existing records are added up but only the newest version of each product in the kept in the resulting table."),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"Merge data",src:a(11163).Z,title:"Merge data",width:"748",height:"285"})))),(0,r.kt)("p",null,"There is no good or bad strategy, the use of one of the strategies above depends on the use case. Some use case examples for each of the strategies are:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Overwrite mode may be useful when you receive every day the list of all product names."),(0,r.kt)("li",{parentName:"ul"},"Append mode may be useful when you receive daily sales."),(0,r.kt)("li",{parentName:"ul"},"Dynamic Partition Overwrite mode may be useful when you ingested the first time a partition, and you need to ingest it again with a different set of data and thus alter only that partition."),(0,r.kt)("li",{parentName:"ul"},"Merge mode may be useful when you receive product updates every day and that you need to keep only the last version of each product.")),(0,r.kt)("h1",{id:"spark-how-to"},"Spark How-to"),(0,r.kt)("p",null,(0,r.kt)("a",{parentName:"p",href:"https://github.com/GoogleCloudDataproc/spark-bigquery-connector"},"Apache Spark SQL connector for Google BigQuery")," makes BigQuery a first class citizen as a source and sink for Spark jobs."),(0,r.kt)("h2",{id:"append-and-overwrite-modes-in-spark"},"Append and Overwrite modes in Spark"),(0,r.kt)("p",null,"BigQuery is supported by Spark as a source and sink through the ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/GoogleCloudDataproc/spark-bigquery-connector"},"Spark BigQuery connector")),(0,r.kt)("p",null,"Spark comes out of the box with the ability to append or overwrite existing data using a predefined save mode:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'\nval incomingDF = ... // Incoming data loaded with the correct schema\nval bqTable = "project-id.dataset.table"\nval saveMode = SaveMode.Overwrite // or SaveMode.Append fot he appending data\nincomingDF.write\n    .mode(saveMode)\n    .partitionBy("date")\n    .format("com.google.cloud.spark.bigquery")\n    .option("table", bqTable)\n    .save()\n\n')),(0,r.kt)("h3",{id:"dynamic-partition-overwrite-mode-in-spark"},"Dynamic Partition Overwrite mode in Spark"),(0,r.kt)("p",null,"To activate dynamic partitioning, you need to set the configuration below before saving the data using the exact same code above:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'spark.conf.set("spark.sql.sources.partitionOverwriteMode","DYNAMIC")\n')),(0,r.kt)("p",null,"Unfortunately, the BigQuery Spark connector does not support this feature (at the time of writing).\nWe need to manually delete the partitions we want to overwrite first and then append the incoming data."),(0,r.kt)("p",null,"Assuming the table is partitioned by the field ",(0,r.kt)("inlineCode",{parentName:"p"},"date")," and the incoming data loaded in the incomingDF dataframe, the code below will\nremove existing partitions that need to be overwritten."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala",metastring:'{3,8} title="Delete partitions that need to be updated"',"{3,8}":!0,title:'"Delete',partitions:!0,that:!0,need:!0,to:!0,be:!0,'updated"':!0},'val incomingDF = ... // Incoming data loaded with the correct schema\nincomingDF\n  .select(date_format(col("date"), "yyyyMMdd").cast("string"))\n  .distinct()\n  .collect()\n  .map(_.getString(0))\n  .foreach { partition =>\n    bigQueryClient.deleteTable(TableId.of(datasetName, s"$table\\$$partition"));\n  }\n')),(0,r.kt)("admonition",{type:"tip"},(0,r.kt)("p",{parentName:"admonition"},"To drop a table partition using the Google Cloud ",(0,r.kt)("inlineCode",{parentName:"p"},"bq")," command line tool, you may use the following syntax:"),(0,r.kt)("pre",{parentName:"admonition"},(0,r.kt)("code",{parentName:"pre",className:"language-shell"},"bq rm -t 'project-id.dataset.table$YYYYMMDD'\n"))),(0,r.kt)("p",null,"We now need to append the incomingDF to mimic the ",(0,r.kt)("inlineCode",{parentName:"p"},"dynamic partition overwrite")," feature:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala",metastring:'{3} title="Append incoming partitions"',"{3}":!0,title:'"Append',incoming:!0,'partitions"':!0},'val incomingDF = ... // Incoming data loaded with the correct schema\nval bqTable = "project-id.dataset.table"\nval saveMode = SaveMode.Append\nincomingDF.write\n    .mode(saveMode)\n    .partitionBy("date")\n    .format("com.google.cloud.spark.bigquery")\n    .option("table", bqTable)\n    .save()\n')),(0,r.kt)("admonition",{type:"caution"},(0,r.kt)("p",{parentName:"admonition"},'The issue with this approach is that if the program crashes during the "appending" of the incoming data, partitions will have been deleted and data would be lost.\nHowever, you can still ingest the same file again in case of failure and the end result will be the same.')),(0,r.kt)("h3",{id:"dynamic-partition-merge-in-spark"},"Dynamic Partition Merge in Spark"),(0,r.kt)("p",null,"When you need to keep the last version of the record for each product, both BigQuery and Databricks (the company behind Spark in case you lived on the moon the last ten years) support\nthe merge SQL statement:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-sql",metastring:'{5,7} title="Merge records using SQL statement"',"{5,7}":!0,title:'"Merge',records:!0,using:!0,SQL:!0,'statement"':!0},"MERGE INTO target_table\nUSING incoming_table\nON target_table.product = incoming_table.product\n    WHEN NOT MATCHED\n        THEN INSERT *\n    WHEN MATCHED AND incoming_table.date > target_table.date THEN\n        UPDATE SET *\n/*\n    WHEN MATCHED AND incoming_table.timestamp <= target_table.timestamp THEN\n        SKIP\n*/\n")),(0,r.kt)("p",null,"Unfortunately the ",(0,r.kt)("inlineCode",{parentName:"p"},"MERGE")," statement is not supported by Apache Spark. It is only supported by Databricks, its commercial version."),(0,r.kt)("p",null,"To do a merge using the Spark BigQuery connector, we need to do it by following the steps below:"),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Step 1: Create a dataframe with all the rows")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"val allRowsDF =\n    incomingDF\n    .unionByName(existingDF)\n")),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"Step 1",src:a(32432).Z,title:"Step 1",width:"752",height:"940"})),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Step 2: group by product and order each product occurrence by date descending")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'val orderingWindow =\n    Window\n      .partitionBy("product")\n      .orderBy(col("date").desc, col("product")))\n\nval orderedDF =\n    allRowsDF\n        .withColumn("rownum", row_number.over(orderingWindow))\n')),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"Step 2",src:a(37386).Z,title:"Step 2",width:"381",height:"325"})),(0,r.kt)("p",null,"In the step 2 above, each product is ordered by date with the most recent one first (descending order).\nWe identify it by the ",(0,r.kt)("inlineCode",{parentName:"p"},"rownum")," column."),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Step 3: Keep the most recent product")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala",metastring:"{3}","{3}":!0},'val toKeepDF =\n    orderedDF\n        .where(col("rownum") === 1)\n        .drop("rownum")\n')),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"Step 3",src:a(14947).Z,title:"Step 3",width:"458",height:"349"})),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Step 4: Overwrite existing partitions with the data we want to keep")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala",metastring:"{3}","{3}":!0},'\nval bqTable = "project-id.dataset.table"\nval saveMode = SaveMode.Overwrite\ntoKeepDF.write\n    .mode(saveMode)\n    .partitionBy("date")\n    .format("com.google.cloud.spark.bigquery")\n    .option("table", bqTable)\n    .save()\n')),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"Step 4",src:a(57442).Z,title:"Step 4",width:"747",height:"278"})),(0,r.kt)("h2",{id:"starlake-how-to"},"Starlake How-to"),(0,r.kt)("p",null,(0,r.kt)("a",{parentName:"p",href:"https://starlake.io"},"Starlake")," is a declarative Ingestion Framework based on YAML description files.",(0,r.kt)("br",null),"\nThe 4 ingestion strategies described above are supported through the settings below:"),(0,r.kt)(i.Z,{groupId:"modes",mdxType:"Tabs"},(0,r.kt)(o.Z,{value:"overwrite",label:"Overwrite",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-json",metastring:'{2} title="application.conf"',"{2}":!0,title:'"application.conf"'},'// When set to "STATIC" all partitions are truncated before data is written in overwrite mode.\nspark.sql.sources.partitionOverwriteMode = "STATIC"\n')),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'{8} title="Schema Definition File"',"{8}":!0,title:'"Schema',Definition:!0,'File"':!0},'     name: "mydb"\n     directory: "..."\n+    metadata:\n    schemas:\n      - name: "mytable"\n        pattern: "data-.*.csv"\n        metadata:\n          mode: "Overwrite"\n        attributes:\n          - name: "date"\n            type: "date"\n            rename: "id"\n          - name: "product"\n            type: "string"\n          - name: "price"\n            type: "decimal"\n')),(0,r.kt)("p",null,(0,r.kt)("a",{parentName:"p",href:"#append-and-overwrite-modes-in-spark"},"See again manual Spark overwrite"))),(0,r.kt)(o.Z,{value:"append",label:"Append",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-json",metastring:'title="application.conf"',title:'"application.conf"'},'// Irrelevant since we are only appending data afterwards\nspark.sql.sources.partitionOverwriteMode = "STATIC" // or "DYNAMIC"\n')),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'{8} title="Schema Definition File"',"{8}":!0,title:'"Schema',Definition:!0,'File"':!0},'     name: "mydb"\n     directory: "..."\n+    metadata:\n    schemas:\n      - name: "mytable"\n        pattern: "data-.*.csv"\n        metadata:\n          mode: "Append"\n        attributes:\n          - name: "date"\n            type: "date"\n            rename: "id"\n          - name: "product"\n            type: "string"\n          - name: "price"\n            type: "decimal"\n')),(0,r.kt)("p",null,(0,r.kt)("a",{parentName:"p",href:"#append-and-overwrite-modes-in-spark"},"See again manual Spark append"))),(0,r.kt)(o.Z,{value:"partitioned-overwrite",label:"Dynamic partition Overwrite",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-json",metastring:'{2} title="application.conf"',"{2}":!0,title:'"application.conf"'},'// When set to "DYNAMIC", only partitions that are  affected will be truncated in overwrite mode.\nspark.sql.sources.partitionOverwriteMode = "DYNAMIC"\n')),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'{8} title="Schema Definition File"',"{8}":!0,title:'"Schema',Definition:!0,'File"':!0},'     name: "mydb"\n     directory: "..."\n+    metadata:\n    schemas:\n      - name: "mytable"\n        pattern: "data-.*.csv"\n        metadata:\n          mode: "Overwrite"\n        attributes:\n          - name: "date"\n            type: "date"\n            rename: "id"\n          - name: "product"\n            type: "string"\n          - name: "price"\n            type: "decimal"\n')),(0,r.kt)("p",null,(0,r.kt)("a",{parentName:"p",href:"#dynamic-partition-overwrite-mode-in-spark"},"See again manual Spark dynamic partition overwrite"))),(0,r.kt)(o.Z,{value:"merge",label:"Merge",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-json",metastring:'title="application.conf"',title:'"application.conf"'},'// Irrelevant since we are truncating manually and  appending data afterwards\nspark.sql.sources.partitionOverwriteMode = "DYNAMIC" // or STATIC\n')),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'{8,9,12,13} title="Schema Definition File"',"{8,9,12,13}":!0,title:'"Schema',Definition:!0,'File"':!0},'     name: "mydb"\n     directory: "..."\n+    metadata:\n    schemas:\n      - name: "mytable"\n        pattern: "data-.*.csv"\n        merge:\n          key:\n            - "product"\n        metadata:\n          mode: "Overwrite"\n          partition:\n            - "date"\n        attributes:\n          - name: "date"\n            type: "date"\n          - name: "product"\n            type: "string"\n          - name: "price"\n            type: "decimal"\n')),(0,r.kt)("p",null,(0,r.kt)("a",{parentName:"p",href:"#dynamic-partition-merge-in-spark"},"See again manual Spark Merge")))),(0,r.kt)("h2",{id:"comments"},"Comments"),(0,r.kt)("p",null,(0,r.kt)("a",{parentName:"p",href:"https://discord.com/channels/833336395430625310/908709208025858079"},"Let's continue the conversation on Discord")))}g.isMDXComponent=!0},33112:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/append-data-6640aca8e10fb60122a396061e2fd835.png"},19893:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/incoming-data-cdda25bb299ec324656b344d1ec0fa69.png"},47808:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/initial-data-11df1668242b934b8701feda5810b4aa.png"},11163:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/merge-data-762cd7b1380a64e2d69fe0db8d2724d6.png"},22502:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/overwrite-data-184774b0ac03484cbc50406abf45f9ce.png"},95517:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/partitioned-overwrite-dda869e262e6573154ce334d2ae1a3a9.png"},32432:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/step1-union-data-bed4397c49e18fa5cac040448e6ca729.png"},37386:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/step2-ordering-data-54433f38583dbe31bf3f12e182ffb14b.png"},14947:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/step3-keep-data-36d7eea05a5e792e351ca7dcc1e020ab.png"},57442:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/step4-save-data-7b050f58bb2d21e98bbf8a88042a0712.png"}}]);