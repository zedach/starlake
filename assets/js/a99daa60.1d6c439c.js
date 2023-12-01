"use strict";(self.webpackChunkstarlake=self.webpackChunkstarlake||[]).push([[9797],{3905:(e,t,n)=>{n.d(t,{Zo:()=>d,kt:()=>k});var r=n(67294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function l(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function o(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},i=Object.keys(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var p=r.createContext({}),s=function(e){var t=r.useContext(p),n=t;return e&&(n="function"==typeof e?e(t):l(l({},t),e)),n},d=function(e){var t=s(e.components);return r.createElement(p.Provider,{value:t},e.children)},u="mdxType",c={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},m=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,i=e.originalType,p=e.parentName,d=o(e,["components","mdxType","originalType","parentName"]),u=s(n),m=a,k=u["".concat(p,".").concat(m)]||u[m]||c[m]||i;return n?r.createElement(k,l(l({ref:t},d),{},{components:n})):r.createElement(k,l({ref:t},d))}));function k(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var i=n.length,l=new Array(i);l[0]=m;var o={};for(var p in t)hasOwnProperty.call(t,p)&&(o[p]=t[p]);o.originalType=e,o[u]="string"==typeof e?e:a,l[1]=o;for(var s=2;s<i;s++)l[s]=n[s];return r.createElement.apply(null,l)}return r.createElement.apply(null,n)}m.displayName="MDXCreateElement"},19401:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>p,contentTitle:()=>l,default:()=>c,frontMatter:()=>i,metadata:()=>o,toc:()=>s});var r=n(87462),a=(n(67294),n(3905));const i={sidebar_position:150,title:"parquet2csv"},l=void 0,o={unversionedId:"cli/parquet2csv",id:"version-0.8.0/cli/parquet2csv",title:"parquet2csv",description:"Synopsis",source:"@site/versioned_docs/version-0.8.0/cli/parquet2csv.md",sourceDirName:"cli",slug:"/cli/parquet2csv",permalink:"/starlake/docs/cli/parquet2csv",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/versioned_docs/version-0.8.0/cli/parquet2csv.md",tags:[],version:"0.8.0",sidebarPosition:150,frontMatter:{sidebar_position:150,title:"parquet2csv"},sidebar:"starlakeSidebar",previous:{title:"metrics",permalink:"/starlake/docs/cli/metrics"},next:{title:"serve",permalink:"/starlake/docs/cli/serve"}},p={},s=[{value:"Synopsis",id:"synopsis",level:2},{value:"Description",id:"description",level:2},{value:"Parameters",id:"parameters",level:2}],d={toc:s},u="wrapper";function c(e){let{components:t,...n}=e;return(0,a.kt)(u,(0,r.Z)({},d,n,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("h2",{id:"synopsis"},"Synopsis"),(0,a.kt)("p",null,(0,a.kt)("strong",{parentName:"p"},"starlake parquet2csv ","[options]")),(0,a.kt)("h2",{id:"description"},"Description"),(0,a.kt)("p",null,"Convert parquet files to CSV.\nThe folder hierarchy should be in the form /input_folder/domain/schema/part",(0,a.kt)("em",{parentName:"p"},".parquet\nOnce converted the csv files are put in the folder /output_folder/domain/schema.csv file\nWhen the specified number of output partitions is 1 then /output_folder/domain/schema.csv is the file containing the data\notherwise, it is a folder containing the part"),".csv files.\nWhen output_folder is not specified, then the input_folder is used a the base output folder."),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-shell"},"starlake parquet2csv\n         --input_dir /tmp/datasets/accepted/\n         --output_dir /tmp/datasets/csv/\n         --domain sales\n         --schema orders\n         --option header=true\n         --option separator=,\n         --partitions 1\n         --write_mode overwrite\n")),(0,a.kt)("h2",{id:"parameters"},"Parameters"),(0,a.kt)("table",null,(0,a.kt)("thead",{parentName:"table"},(0,a.kt)("tr",{parentName:"thead"},(0,a.kt)("th",{parentName:"tr",align:null},"Parameter"),(0,a.kt)("th",{parentName:"tr",align:null},"Cardinality"),(0,a.kt)("th",{parentName:"tr",align:null},"Description"))),(0,a.kt)("tbody",{parentName:"table"},(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--input_dir:",(0,a.kt)("inlineCode",{parentName:"td"},"<value>")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Required")),(0,a.kt)("td",{parentName:"tr",align:null},"Full Path to input directory")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--output_dir:",(0,a.kt)("inlineCode",{parentName:"td"},"<value>")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Optional")),(0,a.kt)("td",{parentName:"tr",align:null},"Full Path to output directory, if not specified, input_dir is used as output dir")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--domain:",(0,a.kt)("inlineCode",{parentName:"td"},"<value>")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Optional")),(0,a.kt)("td",{parentName:"tr",align:null},"Domain name to convert. All schemas in this domain are converted. If not specified, all schemas of all domains are converted")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--schema:",(0,a.kt)("inlineCode",{parentName:"td"},"<value>")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Optional")),(0,a.kt)("td",{parentName:"tr",align:null},"Schema name to convert. If not specified, all schemas are converted.")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--delete_source:",(0,a.kt)("inlineCode",{parentName:"td"},"<value>")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Optional")),(0,a.kt)("td",{parentName:"tr",align:null},"Should we delete source parquet files after conversion ?")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--write_mode:",(0,a.kt)("inlineCode",{parentName:"td"},"<value>")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Optional")),(0,a.kt)("td",{parentName:"tr",align:null},"One of Set(OVERWRITE, APPEND, ERROR_IF_EXISTS, IGNORE)")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--options:",(0,a.kt)("inlineCode",{parentName:"td"},"k1=v1,k2=v2...")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Optional")),(0,a.kt)("td",{parentName:"tr",align:null},"Any Spark option to use (sep, delimiter, quote, quoteAll, escape, header ...)")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--partitions:",(0,a.kt)("inlineCode",{parentName:"td"},"<value>")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Optional")),(0,a.kt)("td",{parentName:"tr",align:null},"How many output partitions")))))}c.isMDXComponent=!0}}]);