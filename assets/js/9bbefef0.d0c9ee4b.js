"use strict";(self.webpackChunkstarlake_docs=self.webpackChunkstarlake_docs||[]).push([[4484],{3905:(e,t,a)=>{a.d(t,{Zo:()=>m,kt:()=>f});var n=a(7294);function r(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function o(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function l(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?o(Object(a),!0).forEach((function(t){r(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):o(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function i(e,t){if(null==e)return{};var a,n,r=function(e,t){if(null==e)return{};var a,n,r={},o=Object.keys(e);for(n=0;n<o.length;n++)a=o[n],t.indexOf(a)>=0||(r[a]=e[a]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(n=0;n<o.length;n++)a=o[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(r[a]=e[a])}return r}var s=n.createContext({}),u=function(e){var t=n.useContext(s),a=t;return e&&(a="function"==typeof e?e(t):l(l({},t),e)),a},m=function(e){var t=u(e.components);return n.createElement(s.Provider,{value:t},e.children)},d="mdxType",c={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},p=n.forwardRef((function(e,t){var a=e.components,r=e.mdxType,o=e.originalType,s=e.parentName,m=i(e,["components","mdxType","originalType","parentName"]),d=u(a),p=r,f=d["".concat(s,".").concat(p)]||d[p]||c[p]||o;return a?n.createElement(f,l(l({ref:t},m),{},{components:a})):n.createElement(f,l({ref:t},m))}));function f(e,t){var a=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=a.length,l=new Array(o);l[0]=p;var i={};for(var s in t)hasOwnProperty.call(t,s)&&(i[s]=t[s]);i.originalType=e,i[d]="string"==typeof e?e:r,l[1]=i;for(var u=2;u<o;u++)l[u]=a[u];return n.createElement.apply(null,l)}return n.createElement.apply(null,a)}p.displayName="MDXCreateElement"},5162:(e,t,a)=>{a.d(t,{Z:()=>l});var n=a(7294),r=a(6010);const o={tabItem:"tabItem_Ymn6"};function l(e){let{children:t,hidden:a,className:l}=e;return n.createElement("div",{role:"tabpanel",className:(0,r.Z)(o.tabItem,l),hidden:a},t)}},4866:(e,t,a)=>{a.d(t,{Z:()=>w});var n=a(7462),r=a(7294),o=a(6010),l=a(2466),i=a(6550),s=a(1980),u=a(7392),m=a(12);function d(e){return function(e){return r.Children.map(e,(e=>{if(!e||(0,r.isValidElement)(e)&&function(e){const{props:t}=e;return!!t&&"object"==typeof t&&"value"in t}(e))return e;throw new Error(`Docusaurus error: Bad <Tabs> child <${"string"==typeof e.type?e.type:e.type.name}>: all children of the <Tabs> component should be <TabItem>, and every <TabItem> should have a unique "value" prop.`)}))?.filter(Boolean)??[]}(e).map((e=>{let{props:{value:t,label:a,attributes:n,default:r}}=e;return{value:t,label:a,attributes:n,default:r}}))}function c(e){const{values:t,children:a}=e;return(0,r.useMemo)((()=>{const e=t??d(a);return function(e){const t=(0,u.l)(e,((e,t)=>e.value===t.value));if(t.length>0)throw new Error(`Docusaurus error: Duplicate values "${t.map((e=>e.value)).join(", ")}" found in <Tabs>. Every value needs to be unique.`)}(e),e}),[t,a])}function p(e){let{value:t,tabValues:a}=e;return a.some((e=>e.value===t))}function f(e){let{queryString:t=!1,groupId:a}=e;const n=(0,i.k6)(),o=function(e){let{queryString:t=!1,groupId:a}=e;if("string"==typeof t)return t;if(!1===t)return null;if(!0===t&&!a)throw new Error('Docusaurus error: The <Tabs> component groupId prop is required if queryString=true, because this value is used as the search param name. You can also provide an explicit value such as queryString="my-search-param".');return a??null}({queryString:t,groupId:a});return[(0,s._X)(o),(0,r.useCallback)((e=>{if(!o)return;const t=new URLSearchParams(n.location.search);t.set(o,e),n.replace({...n.location,search:t.toString()})}),[o,n])]}function h(e){const{defaultValue:t,queryString:a=!1,groupId:n}=e,o=c(e),[l,i]=(0,r.useState)((()=>function(e){let{defaultValue:t,tabValues:a}=e;if(0===a.length)throw new Error("Docusaurus error: the <Tabs> component requires at least one <TabItem> children component");if(t){if(!p({value:t,tabValues:a}))throw new Error(`Docusaurus error: The <Tabs> has a defaultValue "${t}" but none of its children has the corresponding value. Available values are: ${a.map((e=>e.value)).join(", ")}. If you intend to show no default tab, use defaultValue={null} instead.`);return t}const n=a.find((e=>e.default))??a[0];if(!n)throw new Error("Unexpected error: 0 tabValues");return n.value}({defaultValue:t,tabValues:o}))),[s,u]=f({queryString:a,groupId:n}),[d,h]=function(e){let{groupId:t}=e;const a=function(e){return e?`docusaurus.tab.${e}`:null}(t),[n,o]=(0,m.Nk)(a);return[n,(0,r.useCallback)((e=>{a&&o.set(e)}),[a,o])]}({groupId:n}),b=(()=>{const e=s??d;return p({value:e,tabValues:o})?e:null})();(0,r.useLayoutEffect)((()=>{b&&i(b)}),[b]);return{selectedValue:l,selectValue:(0,r.useCallback)((e=>{if(!p({value:e,tabValues:o}))throw new Error(`Can't select invalid tab value=${e}`);i(e),u(e),h(e)}),[u,h,o]),tabValues:o}}var b=a(2389);const y={tabList:"tabList__CuJ",tabItem:"tabItem_LNqP"};function k(e){let{className:t,block:a,selectedValue:i,selectValue:s,tabValues:u}=e;const m=[],{blockElementScrollPositionUntilNextRender:d}=(0,l.o5)(),c=e=>{const t=e.currentTarget,a=m.indexOf(t),n=u[a].value;n!==i&&(d(t),s(n))},p=e=>{let t=null;switch(e.key){case"Enter":c(e);break;case"ArrowRight":{const a=m.indexOf(e.currentTarget)+1;t=m[a]??m[0];break}case"ArrowLeft":{const a=m.indexOf(e.currentTarget)-1;t=m[a]??m[m.length-1];break}}t?.focus()};return r.createElement("ul",{role:"tablist","aria-orientation":"horizontal",className:(0,o.Z)("tabs",{"tabs--block":a},t)},u.map((e=>{let{value:t,label:a,attributes:l}=e;return r.createElement("li",(0,n.Z)({role:"tab",tabIndex:i===t?0:-1,"aria-selected":i===t,key:t,ref:e=>m.push(e),onKeyDown:p,onClick:c},l,{className:(0,o.Z)("tabs__item",y.tabItem,l?.className,{"tabs__item--active":i===t})}),a??t)})))}function v(e){let{lazy:t,children:a,selectedValue:n}=e;const o=(Array.isArray(a)?a:[a]).filter(Boolean);if(t){const e=o.find((e=>e.props.value===n));return e?(0,r.cloneElement)(e,{className:"margin-top--md"}):null}return r.createElement("div",{className:"margin-top--md"},o.map(((e,t)=>(0,r.cloneElement)(e,{key:t,hidden:e.props.value!==n}))))}function g(e){const t=h(e);return r.createElement("div",{className:(0,o.Z)("tabs-container",y.tabList)},r.createElement(k,(0,n.Z)({},e,t)),r.createElement(v,(0,n.Z)({},e,t)))}function w(e){const t=(0,b.Z)();return r.createElement(g,(0,n.Z)({key:String(t)},e))}},4963:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>s,contentTitle:()=>l,default:()=>c,frontMatter:()=>o,metadata:()=>i,toc:()=>u});var n=a(7462),r=(a(7294),a(3905));a(4866),a(5162);const o={sidebar_position:10},l="General",i={unversionedId:"concepts/general",id:"concepts/general",title:"General",description:"Starlake is based on a YAML DSL (Domain Specific Language) to define the data pipelines. All these files are stored in the metadata folder.",source:"@site/docs/concepts/010.general.mdx",sourceDirName:"concepts",slug:"/concepts/general",permalink:"/starlake/docs/next/concepts/general",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/docs/concepts/010.general.mdx",tags:[],version:"current",sidebarPosition:10,frontMatter:{sidebar_position:10},sidebar:"starlakeSidebar",previous:{title:"Concepts",permalink:"/starlake/docs/next/category/concepts"},next:{title:"Extract",permalink:"/starlake/docs/next/concepts/extract"}},s={},u=[{value:"Metadata",id:"metadata",level:2},{value:"Domains",id:"domains",level:2},{value:"Tables",id:"tables",level:2},{value:"Connections",id:"connections",level:2},{value:"Environments",id:"environments",level:2},{value:"Refs",id:"refs",level:2}],m={toc:u},d="wrapper";function c(e){let{components:t,...a}=e;return(0,r.kt)(d,(0,n.Z)({},m,a,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"general"},"General"),(0,r.kt)("p",null,"Starlake is based on a YAML DSL (Domain Specific Language) to define the data pipelines. All these files are stored in the ",(0,r.kt)("inlineCode",{parentName:"p"},"metadata")," folder."),(0,r.kt)("h2",{id:"metadata"},"Metadata"),(0,r.kt)("p",null,"All the information related to a project is stored in the metadata folder. This folder is organized as follows:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"metadata/extract"),": this directory contains all the extraction rules such as which tables and columns need to be extracted from an existing database.")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"metadata/load"),": this directory contains all the information related to load jobs. A load job is a job that load files from disk,\nvalidate the input records and store them in the target datawarehouse")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"metadata/transform"),": this directory contains all the information related to transform jobs.\nA transform job is a job that run against existing tables on the target database and write the result in a target table.\nThese are usually jobs that compute KPIs by aggregating values from different fact tables and or apply specific transformations to do some cleaning.")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"metadata/types"),": this is a companion directory to the metadata/load directory.\nIt contains all the custom types validation you wish to apply on the records loaded from a file.\nWe may require a record column to have a specific length or to verify a specific pattern like an email for example.\nThis allows to go much further than simply checking the input against standard python or java types.")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"metadata/expectations"),": This directory contains a library of expectations we may want to use in our project.\nExpectations are used to validate the data we load into our datawarehouse.\nFor example, we may want to check that the number of records loaded in a table is greater than 0.")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"metadata/connections.sl.yml"),": This file contains all the connections to our datawarehouse, although usually we just have one connection.")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"metadata/refs.sl.yml"),": In your SQL transforms, we may be joining tables coming from different domains or databases.\nIn that case, we normally need to reference our tables by their full names such as: ",(0,r.kt)("inlineCode",{parentName:"p"},"project-id.domain.my-table"),".\nThe refs file allows us to make our query more readable by allowing to map the name ",(0,r.kt)("inlineCode",{parentName:"p"},"my-table")," to its full name.")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"metadata/env.","[SL_ENV]",".sl.yml"),": we may use any of the variables defined in the env files in your extract, load and transforms jobs.\nThe SL_ENV env var activates a specific env file making it possible to assign to variables, values based on the active environment."))),(0,r.kt)("h2",{id:"domains"},"Domains"),(0,r.kt)("p",null,"Whether we load files or apply operations on tables, our load and transform jobs will end up writing to a table hosted in a domain."),(0,r.kt)("p",null,"A domain is a synonym to :"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"a schema in some databases such as Postgres or Snowflake"),(0,r.kt)("li",{parentName:"ul"},"a database in Databricks"),(0,r.kt)("li",{parentName:"ul"},"A dataset in BigQuery.")),(0,r.kt)("p",null,"Since tables are necessarily hosted in a domain, Starlake need to create a domain if it does not exist before it stores the data into the table.\nThe domain name is specified by the folder name where the table is defined.\nFor example, if we have a table defined in the following path: ",(0,r.kt)("inlineCode",{parentName:"p"},"metadata/transform/my-domain/my-table.sl.yml"),", then the domain name will be ",(0,r.kt)("inlineCode",{parentName:"p"},"my-domain"),"."),(0,r.kt)("admonition",{type:"note"},(0,r.kt)("p",{parentName:"admonition"},"We may still overload the domain name in the file ",(0,r.kt)("inlineCode",{parentName:"p"},"metadata/transform/my-domain/_config.sl.yml")," by specifying a different domain name in the ",(0,r.kt)("inlineCode",{parentName:"p"},"name")," property.")),(0,r.kt)("h2",{id:"tables"},"Tables"),(0,r.kt)("p",null,"Any load or transform job that will end up writing to a table ",(0,r.kt)("inlineCode",{parentName:"p"},"my-table")," in the domain ",(0,r.kt)("inlineCode",{parentName:"p"},"my-domain"),"\nis defined in the file ",(0,r.kt)("inlineCode",{parentName:"p"},"metadata/load/my-domain/my-table.sl.yml")," for load jobs and in the file ",(0,r.kt)("inlineCode",{parentName:"p"},"metadata/transform/my-domain/my-table.sl.yml")," for transform jobs."),(0,r.kt)("admonition",{type:"note"},(0,r.kt)("p",{parentName:"admonition"},"We may still overload the table name in the file ",(0,r.kt)("inlineCode",{parentName:"p"},"my-table.sl.yml")," by specifying a different table name in the ",(0,r.kt)("inlineCode",{parentName:"p"},"name")," property.")),(0,r.kt)("h2",{id:"connections"},"Connections"),(0,r.kt)("p",null,"Connections are defined in the file ",(0,r.kt)("inlineCode",{parentName:"p"},"metadata/connections.sl.yml"),". This file contains a list of connections.\nEach connection has a name and a type."),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"The type is the type of the database we want to connect to: JDBC, BigQuery, Databricks, Spark, Hive, Filesystem ect ..."),(0,r.kt)("li",{parentName:"ul"},"The name is the name we will use to reference the connection in our load and transform jobs.")),(0,r.kt)("h2",{id:"environments"},"Environments"),(0,r.kt)("p",null,"Environments are defined in the file ",(0,r.kt)("inlineCode",{parentName:"p"},"metadata/env.[SL_ENV].sl.yml"),". This file contains a list of variables.\nEach variable has a name and a value."),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"The name is the name we will use to reference the variable in our jobs using the syntax ",(0,r.kt)("inlineCode",{parentName:"li"},"{{variable}}")," or ",(0,r.kt)("inlineCode",{parentName:"li"},"${variable}"),"."),(0,r.kt)("li",{parentName:"ul"},"The value is the value of the variable.")),(0,r.kt)("p",null,"We may assign a value to a variable based on the active environment.\nFor example, we may want to use a different database name based on the environment we are running our job in.\nThe file ",(0,r.kt)("inlineCode",{parentName:"p"},"metadata/env.sl.yml")," is always loaded then, if the SL_ENV variable is defined, the file metadata/env.","[SL_ENV]",".sl.yml is also loaded\nand values defined in this file will override the values defined in the file ",(0,r.kt)("inlineCode",{parentName:"p"},"metadata/env.sl.yml"),"."),(0,r.kt)("h2",{id:"refs"},"Refs"),(0,r.kt)("p",null,"Refs are used essentially in SQL transform jobs. They allow to reference a table by its name instead of its full name."),(0,r.kt)("p",null,"For example, if we have a table ",(0,r.kt)("inlineCode",{parentName:"p"},"my-table")," in the domain ",(0,r.kt)("inlineCode",{parentName:"p"},"my-domain"),", we may reference it in our SQL query by using the syntax ",(0,r.kt)("inlineCode",{parentName:"p"},"my-table")," instead of ",(0,r.kt)("inlineCode",{parentName:"p"},"my-domain.my-table")," or ",(0,r.kt)("inlineCode",{parentName:"p"},"my-database.my-domain.my-table"),"."),(0,r.kt)("p",null,"Refs are also useful to implicitly reference a table in a different domain or database based on the environment our job is running in: DEV or PROD for example."))}c.isMDXComponent=!0}}]);