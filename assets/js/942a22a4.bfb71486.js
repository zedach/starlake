"use strict";(self.webpackChunkstarlake=self.webpackChunkstarlake||[]).push([[618],{3905:(e,t,a)=>{a.d(t,{Zo:()=>d,kt:()=>M});var i=a(67294);function n(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function l(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);t&&(i=i.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,i)}return a}function r(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?l(Object(a),!0).forEach((function(t){n(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):l(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function c(e,t){if(null==e)return{};var a,i,n=function(e,t){if(null==e)return{};var a,i,n={},l=Object.keys(e);for(i=0;i<l.length;i++)a=l[i],t.indexOf(a)>=0||(n[a]=e[a]);return n}(e,t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(i=0;i<l.length;i++)a=l[i],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(n[a]=e[a])}return n}var s=i.createContext({}),o=function(e){var t=i.useContext(s),a=t;return e&&(a="function"==typeof e?e(t):r(r({},t),e)),a},d=function(e){var t=o(e.components);return i.createElement(s.Provider,{value:t},e.children)},u="mdxType",I={inlineCode:"code",wrapper:function(e){var t=e.children;return i.createElement(i.Fragment,{},t)}},m=i.forwardRef((function(e,t){var a=e.components,n=e.mdxType,l=e.originalType,s=e.parentName,d=c(e,["components","mdxType","originalType","parentName"]),u=o(a),m=n,M=u["".concat(s,".").concat(m)]||u[m]||I[m]||l;return a?i.createElement(M,r(r({ref:t},d),{},{components:a})):i.createElement(M,r({ref:t},d))}));function M(e,t){var a=arguments,n=t&&t.mdxType;if("string"==typeof e||n){var l=a.length,r=new Array(l);r[0]=m;var c={};for(var s in t)hasOwnProperty.call(t,s)&&(c[s]=t[s]);c.originalType=e,c[u]="string"==typeof e?e:n,r[1]=c;for(var o=2;o<l;o++)r[o]=a[o];return i.createElement.apply(null,r)}return i.createElement.apply(null,a)}m.displayName="MDXCreateElement"},85162:(e,t,a)=>{a.d(t,{Z:()=>r});var i=a(67294),n=a(86010);const l={tabItem:"tabItem_Ymn6"};function r(e){let{children:t,hidden:a,className:r}=e;return i.createElement("div",{role:"tabpanel",className:(0,n.Z)(l.tabItem,r),hidden:a},t)}},74866:(e,t,a)=>{a.d(t,{Z:()=>Z});var i=a(87462),n=a(67294),l=a(86010),r=a(12466),c=a(16550),s=a(91980),o=a(67392),d=a(50012);function u(e){return function(e){return n.Children.map(e,(e=>{if(!e||(0,n.isValidElement)(e)&&function(e){const{props:t}=e;return!!t&&"object"==typeof t&&"value"in t}(e))return e;throw new Error(`Docusaurus error: Bad <Tabs> child <${"string"==typeof e.type?e.type:e.type.name}>: all children of the <Tabs> component should be <TabItem>, and every <TabItem> should have a unique "value" prop.`)}))?.filter(Boolean)??[]}(e).map((e=>{let{props:{value:t,label:a,attributes:i,default:n}}=e;return{value:t,label:a,attributes:i,default:n}}))}function I(e){const{values:t,children:a}=e;return(0,n.useMemo)((()=>{const e=t??u(a);return function(e){const t=(0,o.l)(e,((e,t)=>e.value===t.value));if(t.length>0)throw new Error(`Docusaurus error: Duplicate values "${t.map((e=>e.value)).join(", ")}" found in <Tabs>. Every value needs to be unique.`)}(e),e}),[t,a])}function m(e){let{value:t,tabValues:a}=e;return a.some((e=>e.value===t))}function M(e){let{queryString:t=!1,groupId:a}=e;const i=(0,c.k6)(),l=function(e){let{queryString:t=!1,groupId:a}=e;if("string"==typeof t)return t;if(!1===t)return null;if(!0===t&&!a)throw new Error('Docusaurus error: The <Tabs> component groupId prop is required if queryString=true, because this value is used as the search param name. You can also provide an explicit value such as queryString="my-search-param".');return a??null}({queryString:t,groupId:a});return[(0,s._X)(l),(0,n.useCallback)((e=>{if(!l)return;const t=new URLSearchParams(i.location.search);t.set(l,e),i.replace({...i.location,search:t.toString()})}),[l,i])]}function b(e){const{defaultValue:t,queryString:a=!1,groupId:i}=e,l=I(e),[r,c]=(0,n.useState)((()=>function(e){let{defaultValue:t,tabValues:a}=e;if(0===a.length)throw new Error("Docusaurus error: the <Tabs> component requires at least one <TabItem> children component");if(t){if(!m({value:t,tabValues:a}))throw new Error(`Docusaurus error: The <Tabs> has a defaultValue "${t}" but none of its children has the corresponding value. Available values are: ${a.map((e=>e.value)).join(", ")}. If you intend to show no default tab, use defaultValue={null} instead.`);return t}const i=a.find((e=>e.default))??a[0];if(!i)throw new Error("Unexpected error: 0 tabValues");return i.value}({defaultValue:t,tabValues:l}))),[s,o]=M({queryString:a,groupId:i}),[u,b]=function(e){let{groupId:t}=e;const a=function(e){return e?`docusaurus.tab.${e}`:null}(t),[i,l]=(0,d.Nk)(a);return[i,(0,n.useCallback)((e=>{a&&l.set(e)}),[a,l])]}({groupId:i}),g=(()=>{const e=s??u;return m({value:e,tabValues:l})?e:null})();(0,n.useLayoutEffect)((()=>{g&&c(g)}),[g]);return{selectedValue:r,selectValue:(0,n.useCallback)((e=>{if(!m({value:e,tabValues:l}))throw new Error(`Can't select invalid tab value=${e}`);c(e),o(e),b(e)}),[o,b,l]),tabValues:l}}var g=a(72389);const p={tabList:"tabList__CuJ",tabItem:"tabItem_LNqP"};function N(e){let{className:t,block:a,selectedValue:c,selectValue:s,tabValues:o}=e;const d=[],{blockElementScrollPositionUntilNextRender:u}=(0,r.o5)(),I=e=>{const t=e.currentTarget,a=d.indexOf(t),i=o[a].value;i!==c&&(u(t),s(i))},m=e=>{let t=null;switch(e.key){case"Enter":I(e);break;case"ArrowRight":{const a=d.indexOf(e.currentTarget)+1;t=d[a]??d[0];break}case"ArrowLeft":{const a=d.indexOf(e.currentTarget)-1;t=d[a]??d[d.length-1];break}}t?.focus()};return n.createElement("ul",{role:"tablist","aria-orientation":"horizontal",className:(0,l.Z)("tabs",{"tabs--block":a},t)},o.map((e=>{let{value:t,label:a,attributes:r}=e;return n.createElement("li",(0,i.Z)({role:"tab",tabIndex:c===t?0:-1,"aria-selected":c===t,key:t,ref:e=>d.push(e),onKeyDown:m,onClick:I},r,{className:(0,l.Z)("tabs__item",p.tabItem,r?.className,{"tabs__item--active":c===t})}),a??t)})))}function w(e){let{lazy:t,children:a,selectedValue:i}=e;const l=(Array.isArray(a)?a:[a]).filter(Boolean);if(t){const e=l.find((e=>e.props.value===i));return e?(0,n.cloneElement)(e,{className:"margin-top--md"}):null}return n.createElement("div",{className:"margin-top--md"},l.map(((e,t)=>(0,n.cloneElement)(e,{key:t,hidden:e.props.value!==i}))))}function T(e){const t=b(e);return n.createElement("div",{className:(0,l.Z)("tabs-container",p.tabList)},n.createElement(N,(0,i.Z)({},e,t)),n.createElement(w,(0,i.Z)({},e,t)))}function Z(e){const t=(0,g.Z)();return n.createElement(T,(0,i.Z)({key:String(t)},e))}},1906:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>s,contentTitle:()=>r,default:()=>I,frontMatter:()=>l,metadata:()=>c,toc:()=>o});var i=a(87462),n=(a(67294),a(3905));a(74866),a(85162);const l={},r="How is Starlake different ?",c={unversionedId:"concepts/unique",id:"concepts/unique",title:"How is Starlake different ?",description:"Looking at ELT tools, we can see that they are either:",source:"@site/docs/0100-concepts/009.unique.mdx",sourceDirName:"0100-concepts",slug:"/concepts/unique",permalink:"/starlake/docs/next/concepts/unique",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/docs/0100-concepts/009.unique.mdx",tags:[],version:"current",sidebarPosition:9,frontMatter:{},sidebar:"starlakeSidebar",previous:{title:"Concepts",permalink:"/starlake/docs/next/category/concepts"},next:{title:"General",permalink:"/starlake/docs/next/concepts/general"}},s={},o=[{value:"Extract",id:"extract",level:2},{value:"Load",id:"load",level:2},{value:"Transform",id:"transform",level:2},{value:"Orchestration",id:"orchestration",level:2}],d={toc:o},u="wrapper";function I(e){let{components:t,...l}=e;return(0,n.kt)(u,(0,i.Z)({},d,l,{components:t,mdxType:"MDXLayout"}),(0,n.kt)("h1",{id:"how-is-starlake-different-"},"How is Starlake different ?"),(0,n.kt)("p",null,"Looking at ELT tools, we can see that they are either:"),(0,n.kt)("ul",null,(0,n.kt)("li",{parentName:"ul"},(0,n.kt)("strong",{parentName:"li"},"Code based"),": They require to write code to define the data pipelines. This is the case for example for Databricks or Meltano."),(0,n.kt)("li",{parentName:"ul"},(0,n.kt)("strong",{parentName:"li"},"GUI based"),": They require to use a GUI to define the data pipelines. This is the case for example for Apache NiFi, Airbyte or Fivetran.")),(0,n.kt)("p",null,"Looking at existing data orchestration tools, we can see that they are either:"),(0,n.kt)("ul",null,(0,n.kt)("li",{parentName:"ul"},(0,n.kt)("strong",{parentName:"li"},"Code based"),": They require to write code to define the data pipelines. This is the case for example for Apache Airflow or Dagster."),(0,n.kt)("li",{parentName:"ul"},(0,n.kt)("strong",{parentName:"li"},"GUI based"),": They require to use a GUI to define the data pipelines. This is the case for example for Apache NiFi or StreamSets.")),(0,n.kt)("p",null,"Starlake is different because it is declarative, meaning that we define our data pipelines using a YAML DSL (Domain Specific Language)\ninstead of writing code or using a GUI."),(0,n.kt)("p",null,"These YAMl files are then interpreted by Starlake runtime to execute your end to end data pipelines.\nThe YAML DSL added value is best explained with an example:"),(0,n.kt)("h2",{id:"extract"},"Extract"),(0,n.kt)("p",null,"Let's say we want to extract data from a Postgres Server database on a daily basis"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-yaml"},'extract:\n  connectionRef: "pg-adventure-works-db" # or mssql-adventure-works-db i extracting from SQL Server\n  jdbcSchemas:\n    - schema: "sales"\n      schedule: "0 0 * * *"                     # (optional) cron expression to schedule the extraction\n      tables:\n        - name: "salesorderdetail"              # table name or simple "*" to extract all tables\n          partitionColumn: "salesorderdetailid" # (optional)  you may parallelize the extraction based on this field\n          fetchSize: 100                        # (optional)  the number of rows to fetch at a time\n          timestamp: salesdatetime              # (optional) the timestamp field to use for incremental extraction\n      tableTypes:\n        - "TABLE"\n        #- "VIEW"\n        #- "SYSTEM TABLE"\n        #- "GLOBAL TEMPORARY"\n        #- "LOCAL TEMPORARY"\n        #- "ALIAS"\n        #- "SYNONYM"\n')),(0,n.kt)("p",null,"That's it, we have defined our extraction pipeline."),(0,n.kt)("p",null,"The YAML DSL is self-explanatory and easy to understand. It is also very concise and easy to maintain."),(0,n.kt)("p",null,"Starlake will take care of generating the corresponding DAG (Directed Acyclic Graph) and will run it on a daily basis on your favorite orchestrator (Airflow, Dagster, Prefect, ...)."),(0,n.kt)("h2",{id:"load"},"Load"),(0,n.kt)("p",null,"Let's say we want to load the data extracted from the previous example into datawarehouse"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-yaml"},'---\ntable:\n  pattern: "salesorderdetail.*.psv" # This property is a regular expression that will be used to match the file name.\n  schedule: "when_available"        # (optional) cron expression to schedule the loading\n  metadata:\n    mode: "FILE"\n    format: "CSV"       # (optional) auto-detected if not specified\n    encoding: "UTF-8"\n    withHeader: yes     # (optional) auto-detected if not specified\n    separator: "|"      # (optional) auto-detected if not specified\n    write: "MERGE"\n                        # Please replace it by the adequate file pattern eq. customers-.*.psv if required\n  attributes:           # Description of the fields to recognize\n    - name: "id"        # attribute name and column name in the destination table if no rename attribute is defined\n      type: "string"    # expected type\n      required: false   # Is this field required in the source (false by default, change it accordingly) ?\n      privacy: "NONE"   # Should we encrypt this field before loading to the warehouse (No encryption by default )?\n      ignore: false     # Should this field be excluded (false by default) ?\n    - name: "signup"    # second attribute\n      type: "timestamp" # auto-detected if  specified\n    - name: "contact"\n      type: "string"\n      ...\n  merge:  # (optional) when specified, the merge will be based on this field and only records that have changed will be updated\n    timestamp: signup\n    key: [id]\n')),(0,n.kt)("p",null,"That's it, we have defined our loading pipeline."),(0,n.kt)("p",null,"The YAML DSL is self-explanatory and easy to understand. It is also very concise and easy to maintain."),(0,n.kt)("p",null,"Starlake will take care of generating the corresponding DAG (Directed Acyclic Graph) and will run it upon file\narrival on your orchestrator to validate the input against the YAML schema, apply schema updates\nwhen needed/requested and finally producing a detailed reporting accessible via your favorite BI tool."),(0,n.kt)("p",null,"Starlake support almost any text base file format including FIXED_WIDTH_FIELDS, CSV, TSV, PSV, JSON, XML, AVRO, PARQUET, ORC ..."),(0,n.kt)("h2",{id:"transform"},"Transform"),(0,n.kt)("p",null,"Let's say we want to transform the data extracted from the previous example before loading it into a datawarehouse"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-yaml"},"\ntransform:\n  default:\n    write: OVERWRITE\n  tasks:\n    - name: most_profitable_products\n      write: MERGE\n      merge:\n        timestamp: signup\n        key: [id]\n      sql: |              # based on the merge strategy and the current state,\n          SELECT          # the SQL query will be translated into the appropriate MERGE INTO or INSERT OVERWRITE statement\n            productid,\n            SUM(unitprice * orderqty) AS total_revenue\n            FROM salesorderdetail\n            GROUP BY productid\n            ORDER BY total_revenue DESC\n")),(0,n.kt)("p",null,"That's it, we have defined our transformation pipeline. Starlake will take care of generating the corresponding DAG (Directed Acyclic Graph) and will run it\nwhenever  the tables referenced in the SQL query are updated."),(0,n.kt)("p",null,"Starlake will take care of applying the right merge strategy (INSERT OVERWRITE or MERGE INTO) based on ",(0,n.kt)("inlineCode",{parentName:"p"},"merge")," property and the input /output tables ."),(0,n.kt)("h2",{id:"orchestration"},"Orchestration"),(0,n.kt)("p",null,"Starlake comes with a set of DAG templates that can be used to orchestrate your data pipelines on your favorite orchestrator (Airflow, Dagster, Prefect, ...).\nSimply reference them in your YAML files  and optionally customize them to your needs."),(0,n.kt)("p",null,"The following dependencies are extracted from your SQL query and used to generate the corresponding DAG:\n",(0,n.kt)("img",{src:a(97648).Z,width:"1908",height:"344"})),(0,n.kt)("p",null,"The resulting DAG is shown below:"),(0,n.kt)("p",null,(0,n.kt)("img",{src:a(56740).Z,width:"1509",height:"763"})))}I.isMDXComponent=!0},56740:(e,t,a)=>{a.d(t,{Z:()=>i});const i=a.p+"assets/images/transform-dags-7b3fd2a66f18b84d679a07f8893e6447.png"},97648:(e,t,a)=>{a.d(t,{Z:()=>i});const i="data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiIHN0YW5kYWxvbmU9Im5vIj8+PCFET0NUWVBFIHN2ZyBQVUJMSUMgIi0vL1czQy8vRFREIFNWRyAxLjEvL0VOIiAiaHR0cDovL3d3dy53My5vcmcvR3JhcGhpY3MvU1ZHLzEuMS9EVEQvc3ZnMTEuZHRkIj48IS0tIEdlbmVyYXRlZCBieSBncmFwaHZpeiB2ZXJzaW9uIDIuNDAuMSAoMjAxNjEyMjUuMDMwNCkKIC0tPjwhLS0gVGl0bGU6ICUwIFBhZ2VzOiAxIC0tPjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgd2lkdGg9IjE0MzFwdCIgaGVpZ2h0PSIyNThwdCIgdmlld0JveD0iMC4wMCAwLjAwIDE0MzEuMDAgMjU4LjAwIj4KPGcgaWQ9ImdyYXBoMCIgY2xhc3M9ImdyYXBoIiB0cmFuc2Zvcm09InNjYWxlKDEgMSkgcm90YXRlKDApIHRyYW5zbGF0ZSgzNiAyMjIpIj4KPHRpdGxlPiUwPC90aXRsZT4KPHBvbHlnb24gZmlsbD0iI2ZmZmZmZiIgc3Ryb2tlPSJ0cmFuc3BhcmVudCIgcG9pbnRzPSItMzYsMzYgLTM2LC0yMjIgMTM5NSwtMjIyIDEzOTUsMzYgLTM2LDM2Ii8+CjwhLS0gUHJvZHVjdHNfUHJvZHVjdFByb2ZpdGFiaWxpdHkgLS0+CjxnIGlkPSJub2RlMSIgY2xhc3M9Im5vZGUiPgo8dGl0bGU+UHJvZHVjdHNfUHJvZHVjdFByb2ZpdGFiaWxpdHk8L3RpdGxlPgo8cG9seWdvbiBmaWxsPSIjMDAwMDhiIiBzdHJva2U9InRyYW5zcGFyZW50IiBwb2ludHM9IjgyMiwtNzQgODIyLC0xMTIgMTA0MCwtMTEyIDEwNDAsLTc0IDgyMiwtNzQiLz4KPHBvbHlnb24gZmlsbD0ibm9uZSIgc3Ryb2tlPSIjMDAwMDAwIiBwb2ludHM9IjgyMiwtNzQgODIyLC0xMTIgMTA0MCwtMTEyIDEwNDAsLTc0IDgyMiwtNzQiLz4KPHRleHQgdGV4dC1hbmNob3I9InN0YXJ0IiB4PSI4MzIuNjAxIiB5PSItODkuNCIgZm9udC1mYW1pbHk9IkFyaWFsIiBmb250LXdlaWdodD0iYm9sZCIgZm9udC1zaXplPSIxNC4wMCIgZmlsbD0iI2ZmZmZmZiI+IFByb2R1Y3RzLlByb2R1Y3RQcm9maXRhYmlsaXR5wqDCoDwvdGV4dD4KPC9nPgo8IS0tIHN0YXJiYWtlX1Byb2R1Y3RzIC0tPgo8ZyBpZD0ibm9kZTIiIGNsYXNzPSJub2RlIj4KPHRpdGxlPnN0YXJiYWtlX1Byb2R1Y3RzPC90aXRsZT4KPHBvbHlnb24gZmlsbD0iI2ZmZmZmZiIgc3Ryb2tlPSJ0cmFuc3BhcmVudCIgcG9pbnRzPSIxMTk3LjUsLTc0IDExOTcuNSwtMTEyIDEzNTIuNSwtMTEyIDEzNTIuNSwtNzQgMTE5Ny41LC03NCIvPgo8cG9seWdvbiBmaWxsPSJub25lIiBzdHJva2U9IiMwMDAwMDAiIHBvaW50cz0iMTE5Ny41LC03NCAxMTk3LjUsLTExMiAxMzUyLjUsLTExMiAxMzUyLjUsLTc0IDExOTcuNSwtNzQiLz4KPHRleHQgdGV4dC1hbmNob3I9InN0YXJ0IiB4PSIxMjA4LjA5OTYiIHk9Ii04OS40IiBmb250LWZhbWlseT0iQXJpYWwiIGZvbnQtd2VpZ2h0PSJib2xkIiBmb250LXNpemU9IjE0LjAwIiBmaWxsPSIjMDAwMDAwIj4gc3RhcmJha2UuUHJvZHVjdHPCoMKgPC90ZXh0Pgo8L2c+CjwhLS0gUHJvZHVjdHNfUHJvZHVjdFByb2ZpdGFiaWxpdHkmIzQ1OyZndDtzdGFyYmFrZV9Qcm9kdWN0cyAtLT4KPGcgaWQ9ImVkZ2UyIiBjbGFzcz0iZWRnZSI+Cjx0aXRsZT5Qcm9kdWN0c19Qcm9kdWN0UHJvZml0YWJpbGl0eS0mZ3Q7c3RhcmJha2VfUHJvZHVjdHM8L3RpdGxlPgo8cGF0aCBmaWxsPSJub25lIiBzdHJva2U9IiMwMDAwMDAiIGQ9Ik0xMDQwLjE1NzUsLTkzQzEwODcuNDUyLC05MyAxMTQyLjA4MjIsLTkzIDExODYuNjkyOSwtOTMiLz4KPHBvbHlnb24gZmlsbD0iIzAwMDAwMCIgc3Ryb2tlPSIjMDAwMDAwIiBwb2ludHM9IjExODYuOTI3MSwtOTYuNTAwMSAxMTk2LjkyNzEsLTkzIDExODYuOTI3LC04OS41MDAxIDExODYuOTI3MSwtOTYuNTAwMSIvPgo8L2c+CjwhLS0gc3RhcmJha2VfSW5ncmVkaWVudHMgLS0+CjxnIGlkPSJub2RlMyIgY2xhc3M9Im5vZGUiPgo8dGl0bGU+c3RhcmJha2VfSW5ncmVkaWVudHM8L3RpdGxlPgo8cG9seWdvbiBmaWxsPSIjZmZmZmZmIiBzdHJva2U9InRyYW5zcGFyZW50IiBwb2ludHM9IjExOTAuNSwwIDExOTAuNSwtMzggMTM1OS41LC0zOCAxMzU5LjUsMCAxMTkwLjUsMCIvPgo8cG9seWdvbiBmaWxsPSJub25lIiBzdHJva2U9IiMwMDAwMDAiIHBvaW50cz0iMTE5MC41LDAgMTE5MC41LC0zOCAxMzU5LjUsLTM4IDEzNTkuNSwwIDExOTAuNSwwIi8+Cjx0ZXh0IHRleHQtYW5jaG9yPSJzdGFydCIgeD0iMTIwMS4wOTU0IiB5PSItMTUuNCIgZm9udC1mYW1pbHk9IkFyaWFsIiBmb250LXdlaWdodD0iYm9sZCIgZm9udC1zaXplPSIxNC4wMCIgZmlsbD0iIzAwMDAwMCI+IHN0YXJiYWtlLkluZ3JlZGllbnRzwqDCoDwvdGV4dD4KPC9nPgo8IS0tIFByb2R1Y3RzX1Byb2R1Y3RQcm9maXRhYmlsaXR5JiM0NTsmZ3Q7c3RhcmJha2VfSW5ncmVkaWVudHMgLS0+CjxnIGlkPSJlZGdlMyIgY2xhc3M9ImVkZ2UiPgo8dGl0bGU+UHJvZHVjdHNfUHJvZHVjdFByb2ZpdGFiaWxpdHktJmd0O3N0YXJiYWtlX0luZ3JlZGllbnRzPC90aXRsZT4KPHBhdGggZmlsbD0ibm9uZSIgc3Ryb2tlPSIjMDAwMDAwIiBkPSJNMTAxOS40NTk4LC03My45NDMyQzEwNjguNjU4NCwtNjMuMzQ0MyAxMTI5Ljg4ODMsLTUwLjE1MzYgMTE4MC4wMjExLC0zOS4zNTM1Ii8+Cjxwb2x5Z29uIGZpbGw9IiMwMDAwMDAiIHN0cm9rZT0iIzAwMDAwMCIgcG9pbnRzPSIxMTgwLjg1NDMsLTQyLjc1NDQgMTE4OS44OTI5LC0zNy4yMjY5IDExNzkuMzgsLTM1LjkxMTQgMTE4MC44NTQzLC00Mi43NTQ0Ii8+CjwvZz4KPCEtLSBzdGFyYmFrZV9PcmRlcnMgLS0+CjxnIGlkPSJub2RlNiIgY2xhc3M9Im5vZGUiPgo8dGl0bGU+c3RhcmJha2VfT3JkZXJzPC90aXRsZT4KPHBvbHlnb24gZmlsbD0iI2ZmZmZmZiIgc3Ryb2tlPSJ0cmFuc3BhcmVudCIgcG9pbnRzPSIxMjAzLjUsLTE0OCAxMjAzLjUsLTE4NiAxMzQ2LjUsLTE4NiAxMzQ2LjUsLTE0OCAxMjAzLjUsLTE0OCIvPgo8cG9seWdvbiBmaWxsPSJub25lIiBzdHJva2U9IiMwMDAwMDAiIHBvaW50cz0iMTIwMy41LC0xNDggMTIwMy41LC0xODYgMTM0Ni41LC0xODYgMTM0Ni41LC0xNDggMTIwMy41LC0xNDgiLz4KPHRleHQgdGV4dC1hbmNob3I9InN0YXJ0IiB4PSIxMjE0LjMyODkiIHk9Ii0xNjMuNCIgZm9udC1mYW1pbHk9IkFyaWFsIiBmb250LXdlaWdodD0iYm9sZCIgZm9udC1zaXplPSIxNC4wMCIgZmlsbD0iIzAwMDAwMCI+IHN0YXJiYWtlLk9yZGVyc8KgwqA8L3RleHQ+CjwvZz4KPCEtLSBQcm9kdWN0c19Qcm9kdWN0UHJvZml0YWJpbGl0eSYjNDU7Jmd0O3N0YXJiYWtlX09yZGVycyAtLT4KPGcgaWQ9ImVkZ2UxIiBjbGFzcz0iZWRnZSI+Cjx0aXRsZT5Qcm9kdWN0c19Qcm9kdWN0UHJvZml0YWJpbGl0eS0mZ3Q7c3RhcmJha2VfT3JkZXJzPC90aXRsZT4KPHBhdGggZmlsbD0ibm9uZSIgc3Ryb2tlPSIjMDAwMDAwIiBkPSJNMTAxOS40NTk4LC0xMTIuMDU2OEMxMDcyLjk2MjcsLTEyMy41ODI5IDExNDAuNjk0MSwtMTM4LjE3NDMgMTE5Mi45MDg5LC0xNDkuNDIyOSIvPgo8cG9seWdvbiBmaWxsPSIjMDAwMDAwIiBzdHJva2U9IiMwMDAwMDAiIHBvaW50cz0iMTE5Mi4yOTAzLC0xNTIuODY5OCAxMjAyLjgwMzIsLTE1MS41NTQ0IDExOTMuNzY0NiwtMTQ2LjAyNjggMTE5Mi4yOTAzLC0xNTIuODY5OCIvPgo8L2c+CjwhLS0gUHJvZHVjdHNfUHJvZHVjdFBlcmZvcm1hbmNlIC0tPgo8ZyBpZD0ibm9kZTQiIGNsYXNzPSJub2RlIj4KPHRpdGxlPlByb2R1Y3RzX1Byb2R1Y3RQZXJmb3JtYW5jZTwvdGl0bGU+Cjxwb2x5Z29uIGZpbGw9IiMwMDAwOGIiIHN0cm9rZT0idHJhbnNwYXJlbnQiIHBvaW50cz0iODE2LC0xNDggODE2LC0xODYgMTA0NiwtMTg2IDEwNDYsLTE0OCA4MTYsLTE0OCIvPgo8cG9seWdvbiBmaWxsPSJub25lIiBzdHJva2U9IiMwMDAwMDAiIHBvaW50cz0iODE2LC0xNDggODE2LC0xODYgMTA0NiwtMTg2IDEwNDYsLTE0OCA4MTYsLTE0OCIvPgo8dGV4dCB0ZXh0LWFuY2hvcj0ic3RhcnQiIHg9IjgyNi43NjE2IiB5PSItMTYzLjQiIGZvbnQtZmFtaWx5PSJBcmlhbCIgZm9udC13ZWlnaHQ9ImJvbGQiIGZvbnQtc2l6ZT0iMTQuMDAiIGZpbGw9IiNmZmZmZmYiPiBQcm9kdWN0cy5Qcm9kdWN0UGVyZm9ybWFuY2XCoMKgPC90ZXh0Pgo8L2c+CjwhLS0gUHJvZHVjdHNfUHJvZHVjdFBlcmZvcm1hbmNlJiM0NTsmZ3Q7c3RhcmJha2VfUHJvZHVjdHMgLS0+CjxnIGlkPSJlZGdlNSIgY2xhc3M9ImVkZ2UiPgo8dGl0bGU+UHJvZHVjdHNfUHJvZHVjdFBlcmZvcm1hbmNlLSZndDtzdGFyYmFrZV9Qcm9kdWN0czwvdGl0bGU+CjxwYXRoIGZpbGw9Im5vbmUiIHN0cm9rZT0iIzAwMDAwMCIgZD0iTTEwMTkuNDU5OCwtMTQ3Ljk0MzJDMTA3MC44OTQ3LC0xMzYuODYyNiAxMTM1LjQ3OTUsLTEyMi45NDkxIDExODYuNzg2NSwtMTExLjg5NjEiLz4KPHBvbHlnb24gZmlsbD0iIzAwMDAwMCIgc3Ryb2tlPSIjMDAwMDAwIiBwb2ludHM9IjExODcuODI0NCwtMTE1LjI1MjkgMTE5Ni44NjMsLTEwOS43MjUzIDExODYuMzUwMSwtMTA4LjQwOTggMTE4Ny44MjQ0LC0xMTUuMjUyOSIvPgo8L2c+CjwhLS0gUHJvZHVjdHNfUHJvZHVjdFBlcmZvcm1hbmNlJiM0NTsmZ3Q7c3RhcmJha2VfT3JkZXJzIC0tPgo8ZyBpZD0iZWRnZTQiIGNsYXNzPSJlZGdlIj4KPHRpdGxlPlByb2R1Y3RzX1Byb2R1Y3RQZXJmb3JtYW5jZS0mZ3Q7c3RhcmJha2VfT3JkZXJzPC90aXRsZT4KPHBhdGggZmlsbD0ibm9uZSIgc3Ryb2tlPSIjMDAwMDAwIiBkPSJNMTA0Ni4zMzYxLC0xNjdDMTA5NC4zNDM0LC0xNjcgMTE0OC45NjU5LC0xNjcgMTE5Mi43Mjc5LC0xNjciLz4KPHBvbHlnb24gZmlsbD0iIzAwMDAwMCIgc3Ryb2tlPSIjMDAwMDAwIiBwb2ludHM9IjExOTIuNzQ4NSwtMTcwLjUwMDEgMTIwMi43NDg1LC0xNjcgMTE5Mi43NDg0LC0xNjMuNTAwMSAxMTkyLjc0ODUsLTE3MC41MDAxIi8+CjwvZz4KPCEtLSBQcm9kdWN0c19Ub3BTZWxsaW5nUHJvZml0YWJsZVByb2R1Y3RzIC0tPgo8ZyBpZD0ibm9kZTUiIGNsYXNzPSJub2RlIj4KPHRpdGxlPlByb2R1Y3RzX1RvcFNlbGxpbmdQcm9maXRhYmxlUHJvZHVjdHM8L3RpdGxlPgo8cG9seWdvbiBmaWxsPSIjMDAwMDhiIiBzdHJva2U9InRyYW5zcGFyZW50IiBwb2ludHM9IjAsLTExMSAwLC0xNDkgMjgyLC0xNDkgMjgyLC0xMTEgMCwtMTExIi8+Cjxwb2x5Z29uIGZpbGw9Im5vbmUiIHN0cm9rZT0iIzAwMDAwMCIgcG9pbnRzPSIwLC0xMTEgMCwtMTQ5IDI4MiwtMTQ5IDI4MiwtMTExIDAsLTExMSIvPgo8dGV4dCB0ZXh0LWFuY2hvcj0ic3RhcnQiIHg9IjEwLjcwMDYiIHk9Ii0xMjYuNCIgZm9udC1mYW1pbHk9IkFyaWFsIiBmb250LXdlaWdodD0iYm9sZCIgZm9udC1zaXplPSIxNC4wMCIgZmlsbD0iI2ZmZmZmZiI+IFByb2R1Y3RzLlRvcFNlbGxpbmdQcm9maXRhYmxlUHJvZHVjdHPCoMKgPC90ZXh0Pgo8L2c+CjwhLS0gUHJvZHVjdHNfTW9zdFByb2ZpdGFibGVQcm9kdWN0cyAtLT4KPGcgaWQ9Im5vZGU3IiBjbGFzcz0ibm9kZSI+Cjx0aXRsZT5Qcm9kdWN0c19Nb3N0UHJvZml0YWJsZVByb2R1Y3RzPC90aXRsZT4KPHBvbHlnb24gZmlsbD0iIzAwMDA4YiIgc3Ryb2tlPSJ0cmFuc3BhcmVudCIgcG9pbnRzPSI0MjYsLTc0IDQyNiwtMTEyIDY3MiwtMTEyIDY3MiwtNzQgNDI2LC03NCIvPgo8cG9seWdvbiBmaWxsPSJub25lIiBzdHJva2U9IiMwMDAwMDAiIHBvaW50cz0iNDI2LC03NCA0MjYsLTExMiA2NzIsLTExMiA2NzIsLTc0IDQyNiwtNzQiLz4KPHRleHQgdGV4dC1hbmNob3I9InN0YXJ0IiB4PSI0MzYuNTk0NyIgeT0iLTg5LjQiIGZvbnQtZmFtaWx5PSJBcmlhbCIgZm9udC13ZWlnaHQ9ImJvbGQiIGZvbnQtc2l6ZT0iMTQuMDAiIGZpbGw9IiNmZmZmZmYiPiBQcm9kdWN0cy5Nb3N0UHJvZml0YWJsZVByb2R1Y3RzwqDCoDwvdGV4dD4KPC9nPgo8IS0tIFByb2R1Y3RzX1RvcFNlbGxpbmdQcm9maXRhYmxlUHJvZHVjdHMmIzQ1OyZndDtQcm9kdWN0c19Nb3N0UHJvZml0YWJsZVByb2R1Y3RzIC0tPgo8ZyBpZD0iZWRnZTciIGNsYXNzPSJlZGdlIj4KPHRpdGxlPlByb2R1Y3RzX1RvcFNlbGxpbmdQcm9maXRhYmxlUHJvZHVjdHMtJmd0O1Byb2R1Y3RzX01vc3RQcm9maXRhYmxlUHJvZHVjdHM8L3RpdGxlPgo8cGF0aCBmaWxsPSJub25lIiBzdHJva2U9IiMwMDAwMDAiIGQ9Ik0yODIuMjY3NiwtMTE3LjE4OUMzMjUuNTM4NCwtMTEzLjI2NDkgMzcyLjk4MDcsLTEwOC45NjI1IDQxNS43MDIxLC0xMDUuMDg4MyIvPgo8cG9seWdvbiBmaWxsPSIjMDAwMDAwIiBzdHJva2U9IiMwMDAwMDAiIHBvaW50cz0iNDE2LjIzODMsLTEwOC41NTQxIDQyNS44ODEzLC0xMDQuMTY1MiA0MTUuNjA2LC0xMDEuNTgyNyA0MTYuMjM4MywtMTA4LjU1NDEiLz4KPC9nPgo8IS0tIFByb2R1Y3RzX1RvcFNlbGxpbmdQcm9kdWN0cyAtLT4KPGcgaWQ9Im5vZGU4IiBjbGFzcz0ibm9kZSI+Cjx0aXRsZT5Qcm9kdWN0c19Ub3BTZWxsaW5nUHJvZHVjdHM8L3RpdGxlPgo8cG9seWdvbiBmaWxsPSIjMDAwMDhiIiBzdHJva2U9InRyYW5zcGFyZW50IiBwb2ludHM9IjQzOCwtMTQ4IDQzOCwtMTg2IDY2MSwtMTg2IDY2MSwtMTQ4IDQzOCwtMTQ4Ii8+Cjxwb2x5Z29uIGZpbGw9Im5vbmUiIHN0cm9rZT0iIzAwMDAwMCIgcG9pbnRzPSI0MzgsLTE0OCA0MzgsLTE4NiA2NjEsLTE4NiA2NjEsLTE0OCA0MzgsLTE0OCIvPgo8dGV4dCB0ZXh0LWFuY2hvcj0ic3RhcnQiIHg9IjQ0OC43NjE2IiB5PSItMTYzLjQiIGZvbnQtZmFtaWx5PSJBcmlhbCIgZm9udC13ZWlnaHQ9ImJvbGQiIGZvbnQtc2l6ZT0iMTQuMDAiIGZpbGw9IiNmZmZmZmYiPiBQcm9kdWN0cy5Ub3BTZWxsaW5nUHJvZHVjdHPCoMKgPC90ZXh0Pgo8L2c+CjwhLS0gUHJvZHVjdHNfVG9wU2VsbGluZ1Byb2ZpdGFibGVQcm9kdWN0cyYjNDU7Jmd0O1Byb2R1Y3RzX1RvcFNlbGxpbmdQcm9kdWN0cyAtLT4KPGcgaWQ9ImVkZ2U2IiBjbGFzcz0iZWRnZSI+Cjx0aXRsZT5Qcm9kdWN0c19Ub3BTZWxsaW5nUHJvZml0YWJsZVByb2R1Y3RzLSZndDtQcm9kdWN0c19Ub3BTZWxsaW5nUHJvZHVjdHM8L3RpdGxlPgo8cGF0aCBmaWxsPSJub25lIiBzdHJva2U9IiMwMDAwMDAiIGQ9Ik0yODIuMjY3NiwtMTQyLjgxMUMzMjkuNDY3LC0xNDcuMDkxNCAzODEuNjI5NiwtMTUxLjgyMTggNDI3LjIxNDgsLTE1NS45NTU4Ii8+Cjxwb2x5Z29uIGZpbGw9IiMwMDAwMDAiIHN0cm9rZT0iIzAwMDAwMCIgcG9pbnRzPSI0MjcuMTY0NSwtMTU5LjQ2NTUgNDM3LjQzOTgsLTE1Ni44ODMgNDI3Ljc5NjgsLTE1Mi40OTQxIDQyNy4xNjQ1LC0xNTkuNDY1NSIvPgo8L2c+CjwhLS0gUHJvZHVjdHNfTW9zdFByb2ZpdGFibGVQcm9kdWN0cyYjNDU7Jmd0O1Byb2R1Y3RzX1Byb2R1Y3RQcm9maXRhYmlsaXR5IC0tPgo8ZyBpZD0iZWRnZTgiIGNsYXNzPSJlZGdlIj4KPHRpdGxlPlByb2R1Y3RzX01vc3RQcm9maXRhYmxlUHJvZHVjdHMtJmd0O1Byb2R1Y3RzX1Byb2R1Y3RQcm9maXRhYmlsaXR5PC90aXRsZT4KPHBhdGggZmlsbD0ibm9uZSIgc3Ryb2tlPSIjMDAwMDAwIiBkPSJNNjcyLjIzMzMsLTkzQzcxNi45Mzg5LC05MyA3NjcuMzM1NCwtOTMgODExLjY5NjEsLTkzIi8+Cjxwb2x5Z29uIGZpbGw9IiMwMDAwMDAiIHN0cm9rZT0iIzAwMDAwMCIgcG9pbnRzPSI4MTEuOTQ3MSwtOTYuNTAwMSA4MjEuOTQ3MSwtOTMgODExLjk0NywtODkuNTAwMSA4MTEuOTQ3MSwtOTYuNTAwMSIvPgo8L2c+CjwhLS0gUHJvZHVjdHNfVG9wU2VsbGluZ1Byb2R1Y3RzJiM0NTsmZ3Q7UHJvZHVjdHNfUHJvZHVjdFBlcmZvcm1hbmNlIC0tPgo8ZyBpZD0iZWRnZTkiIGNsYXNzPSJlZGdlIj4KPHRpdGxlPlByb2R1Y3RzX1RvcFNlbGxpbmdQcm9kdWN0cy0mZ3Q7UHJvZHVjdHNfUHJvZHVjdFBlcmZvcm1hbmNlPC90aXRsZT4KPHBhdGggZmlsbD0ibm9uZSIgc3Ryb2tlPSIjMDAwMDAwIiBkPSJNNjYwLjUxNzEsLTE2N0M3MDUuOTYxMywtMTY3IDc1OC43MTE5LC0xNjcgODA1LjQ5NzIsLTE2NyIvPgo8cG9seWdvbiBmaWxsPSIjMDAwMDAwIiBzdHJva2U9IiMwMDAwMDAiIHBvaW50cz0iODA1LjY5NDIsLTE3MC41MDAxIDgxNS42OTQyLC0xNjcgODA1LjY5NDIsLTE2My41MDAxIDgwNS42OTQyLC0xNzAuNTAwMSIvPgo8L2c+CjwvZz4KPC9zdmc+"}}]);