"use strict";(self.webpackChunkstarlake=self.webpackChunkstarlake||[]).push([[3854],{3905:(t,e,a)=>{a.d(e,{Zo:()=>m,kt:()=>k});var n=a(67294);function r(t,e,a){return e in t?Object.defineProperty(t,e,{value:a,enumerable:!0,configurable:!0,writable:!0}):t[e]=a,t}function l(t,e){var a=Object.keys(t);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(t);e&&(n=n.filter((function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable}))),a.push.apply(a,n)}return a}function i(t){for(var e=1;e<arguments.length;e++){var a=null!=arguments[e]?arguments[e]:{};e%2?l(Object(a),!0).forEach((function(e){r(t,e,a[e])})):Object.getOwnPropertyDescriptors?Object.defineProperties(t,Object.getOwnPropertyDescriptors(a)):l(Object(a)).forEach((function(e){Object.defineProperty(t,e,Object.getOwnPropertyDescriptor(a,e))}))}return t}function d(t,e){if(null==t)return{};var a,n,r=function(t,e){if(null==t)return{};var a,n,r={},l=Object.keys(t);for(n=0;n<l.length;n++)a=l[n],e.indexOf(a)>=0||(r[a]=t[a]);return r}(t,e);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(t);for(n=0;n<l.length;n++)a=l[n],e.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(t,a)&&(r[a]=t[a])}return r}var o=n.createContext({}),p=function(t){var e=n.useContext(o),a=e;return t&&(a="function"==typeof t?t(e):i(i({},e),t)),a},m=function(t){var e=p(t.components);return n.createElement(o.Provider,{value:e},t.children)},c="mdxType",s={inlineCode:"code",wrapper:function(t){var e=t.children;return n.createElement(n.Fragment,{},e)}},u=n.forwardRef((function(t,e){var a=t.components,r=t.mdxType,l=t.originalType,o=t.parentName,m=d(t,["components","mdxType","originalType","parentName"]),c=p(a),u=r,k=c["".concat(o,".").concat(u)]||c[u]||s[u]||l;return a?n.createElement(k,i(i({ref:e},m),{},{components:a})):n.createElement(k,i({ref:e},m))}));function k(t,e){var a=arguments,r=e&&e.mdxType;if("string"==typeof t||r){var l=a.length,i=new Array(l);i[0]=u;var d={};for(var o in e)hasOwnProperty.call(e,o)&&(d[o]=e[o]);d.originalType=t,d[c]="string"==typeof t?t:r,i[1]=d;for(var p=2;p<l;p++)i[p]=a[p];return n.createElement.apply(null,i)}return n.createElement.apply(null,a)}u.displayName="MDXCreateElement"},59836:(t,e,a)=>{a.r(e),a.d(e,{assets:()=>o,contentTitle:()=>i,default:()=>s,frontMatter:()=>l,metadata:()=>d,toc:()=>p});var n=a(87462),r=(a(67294),a(3905));const l={sidebar_position:50,title:"extract-data"},i=void 0,d={unversionedId:"cli/extract-data",id:"version-1.0.0/cli/extract-data",title:"extract-data",description:"Synopsis",source:"@site/versioned_docs/version-1.0.0/0800-cli/extract-data.md",sourceDirName:"0800-cli",slug:"/cli/extract-data",permalink:"/starlake/docs/cli/extract-data",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/versioned_docs/version-1.0.0/0800-cli/extract-data.md",tags:[],version:"1.0.0",sidebarPosition:50,frontMatter:{sidebar_position:50,title:"extract-data"},sidebar:"starlakeSidebar",previous:{title:"esload",permalink:"/starlake/docs/cli/esload"},next:{title:"extract-schema",permalink:"/starlake/docs/cli/extract-schema"}},o={},p=[{value:"Synopsis",id:"synopsis",level:2},{value:"Description",id:"description",level:2},{value:"Parameters",id:"parameters",level:2}],m={toc:p},c="wrapper";function s(t){let{components:e,...a}=t;return(0,r.kt)(c,(0,n.Z)({},m,a,{components:e,mdxType:"MDXLayout"}),(0,r.kt)("h2",{id:"synopsis"},"Synopsis"),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"starlake extract-data ","[options]")),(0,r.kt)("h2",{id:"description"},"Description"),(0,r.kt)("p",null,"Extract data from any database defined in mapping file."),(0,r.kt)("p",null,"Extraction is done in parallel by default and use all the available processors. It can be changed using ",(0,r.kt)("inlineCode",{parentName:"p"},"parallelism")," CLI config.\nExtraction of a table can be divided in smaller chunk and fetched in parallel by defining partitionColumn and its numPartitions."),(0,r.kt)("h1",{id:"examples"},"Examples"),(0,r.kt)("p",null,"Objective: Extract data"),(0,r.kt)("p",null,"  starlake.sh extract-data --config my-config --output-dir $PWD/output"),(0,r.kt)("p",null,'Objective: Plan to fetch all data but with different scheduling (once a day for all and twice a day for some) with failure recovery like behavior.\nstarlake.sh extract-data --config my-config --output-dir $PWD/output --includeSchemas aSchema\n--includeTables table1RefreshedTwiceADay,table2RefreshedTwiceADay --ifExtractedBefore "2023-04-21 12:00:00"\n--clean'),(0,r.kt)("h2",{id:"parameters"},"Parameters"),(0,r.kt)("table",null,(0,r.kt)("thead",{parentName:"table"},(0,r.kt)("tr",{parentName:"thead"},(0,r.kt)("th",{parentName:"tr",align:null},"Parameter"),(0,r.kt)("th",{parentName:"tr",align:null},"Cardinality"),(0,r.kt)("th",{parentName:"tr",align:null},"Description"))),(0,r.kt)("tbody",{parentName:"table"},(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--mapping:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"Deprecated. Use config instead")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--config:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Required")),(0,r.kt)("td",{parentName:"tr",align:null},"Database tables & connection info")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--limit:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"Limit number of records")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--numPartitions:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"parallelism level regarding partitionned tables")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--parallelism:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"parallelism level of the extraction process. By default equals to the available cores: 10")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--clean:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"Clean all files of table only when it is extracted.")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--output-dir:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Required")),(0,r.kt)("td",{parentName:"tr",align:null},"Where to output csv files")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--fullExport:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"Force full export to all tables")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--ifExtractedBefore:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"DateTime to compare with the last beginning extraction dateTime. If it is before that date, extraction is done else skipped.")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--cleanOnExtract:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"Deprecated. Use --clean instead.")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--includeSchemas:",(0,r.kt)("inlineCode",{parentName:"td"},"schema1,schema2")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"Domains to include during extraction.")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--excludeSchemas:",(0,r.kt)("inlineCode",{parentName:"td"},"schema1,schema2...")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"Domains to exclude during extraction. if ",(0,r.kt)("inlineCode",{parentName:"td"},"include-domains")," is defined, this config is ignored.")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--includeTables:",(0,r.kt)("inlineCode",{parentName:"td"},"table1,table2,table3...")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"Schemas to include during extraction.")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--excludeTables:",(0,r.kt)("inlineCode",{parentName:"td"},"table1,table2,table3...")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"Schemas to exclude during extraction. if ",(0,r.kt)("inlineCode",{parentName:"td"},"include-schemas")," is defined, this config is ignored.")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null})))))}s.isMDXComponent=!0}}]);