"use strict";(self.webpackChunkstarlake_docs=self.webpackChunkstarlake_docs||[]).push([[5292],{3905:(e,t,a)=>{a.d(t,{Zo:()=>c,kt:()=>k});var r=a(7294);function n(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function l(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,r)}return a}function i(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?l(Object(a),!0).forEach((function(t){n(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):l(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function o(e,t){if(null==e)return{};var a,r,n=function(e,t){if(null==e)return{};var a,r,n={},l=Object.keys(e);for(r=0;r<l.length;r++)a=l[r],t.indexOf(a)>=0||(n[a]=e[a]);return n}(e,t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(r=0;r<l.length;r++)a=l[r],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(n[a]=e[a])}return n}var p=r.createContext({}),s=function(e){var t=r.useContext(p),a=t;return e&&(a="function"==typeof e?e(t):i(i({},t),e)),a},c=function(e){var t=s(e.components);return r.createElement(p.Provider,{value:t},e.children)},m="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},d=r.forwardRef((function(e,t){var a=e.components,n=e.mdxType,l=e.originalType,p=e.parentName,c=o(e,["components","mdxType","originalType","parentName"]),m=s(a),d=n,k=m["".concat(p,".").concat(d)]||m[d]||u[d]||l;return a?r.createElement(k,i(i({ref:t},c),{},{components:a})):r.createElement(k,i({ref:t},c))}));function k(e,t){var a=arguments,n=t&&t.mdxType;if("string"==typeof e||n){var l=a.length,i=new Array(l);i[0]=d;var o={};for(var p in t)hasOwnProperty.call(t,p)&&(o[p]=t[p]);o.originalType=e,o[m]="string"==typeof e?e:n,i[1]=o;for(var s=2;s<l;s++)i[s]=a[s];return r.createElement.apply(null,i)}return r.createElement.apply(null,a)}d.displayName="MDXCreateElement"},721:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>p,contentTitle:()=>i,default:()=>u,frontMatter:()=>l,metadata:()=>o,toc:()=>s});var r=a(7462),n=(a(7294),a(3905));const l={sidebar_position:40,title:"extract-script"},i=void 0,o={unversionedId:"cli/extract",id:"cli/extract",title:"extract-script",description:"Synopsis",source:"@site/docs/cli/extract.md",sourceDirName:"cli",slug:"/cli/extract",permalink:"/starlake/docs/next/cli/extract",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/docs/cli/extract.md",tags:[],version:"current",sidebarPosition:40,frontMatter:{sidebar_position:40,title:"extract-script"},sidebar:"starlakeSidebar",previous:{title:"cnxload",permalink:"/starlake/docs/next/cli/cnxload"},next:{title:"esload",permalink:"/starlake/docs/next/cli/esload"}},p={},s=[{value:"Synopsis",id:"synopsis",level:2},{value:"Description",id:"description",level:2},{value:"Parameters",id:"parameters",level:2}],c={toc:s},m="wrapper";function u(e){let{components:t,...a}=e;return(0,n.kt)(m,(0,r.Z)({},c,a,{components:t,mdxType:"MDXLayout"}),(0,n.kt)("h2",{id:"synopsis"},"Synopsis"),(0,n.kt)("p",null,(0,n.kt)("strong",{parentName:"p"},"starlake extract-script ","[options]")),(0,n.kt)("h2",{id:"description"},"Description"),(0,n.kt)("p",null,"For domain extraction, the schemas should at least, specify :"),(0,n.kt)("ul",null,(0,n.kt)("li",{parentName:"ul"},"a table name (schemas.name)"),(0,n.kt)("li",{parentName:"ul"},"a file pattern (schemas.pattern) which is used as the export file base name"),(0,n.kt)("li",{parentName:"ul"},"a write mode (schemas.metadata.write): APPEND or OVERWRITE"),(0,n.kt)("li",{parentName:"ul"},"a delta column (schemas.merge.timestamp) if in APPEND mode : the default column which is used to determine new rows for each exports"),(0,n.kt)("li",{parentName:"ul"},"the columns to extract (schemas.attributes.name*)")),(0,n.kt)("p",null,"You also have to provide a Mustache (",(0,n.kt)("a",{parentName:"p",href:"http://mustache.github.io/mustache.5.html"},"http://mustache.github.io/mustache.5.html"),") template file."),(0,n.kt)("p",null,"In there you'll write your extraction export process (sqlplus for Oracle, pgsql for PostgreSQL as an example).\nIn that template you can use the following parameters:"),(0,n.kt)("ul",null,(0,n.kt)("li",{parentName:"ul"},"table_name  -> the table to export"),(0,n.kt)("li",{parentName:"ul"},"delimiter   -> the resulting dsv file delimiter"),(0,n.kt)("li",{parentName:"ul"},"columns     -> the columns to export\ncolumns is a Mustache map, it gives you access, for each column, to:",(0,n.kt)("ul",{parentName:"li"},(0,n.kt)("li",{parentName:"ul"},"name               -> the column name"),(0,n.kt)("li",{parentName:"ul"},'trailing_col_char  -> the separator to append to the column (, if there are more columns to come, "" otherwise)\nHere is an example how to use it in a template:')))),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-sql"},"                              SELECT\n                              {{#columns}}\n                              TO_CHAR({{name}}){{trailing_col_char}}\n                              {{/columns}}\n                              FROM\n                              {{table_name}};\n")),(0,n.kt)("p",null," full_export -> if the export is a full or delta export (the logic is to be implemented in your script)"),(0,n.kt)("h2",{id:"parameters"},"Parameters"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:null},"Parameter"),(0,n.kt)("th",{parentName:"tr",align:null},"Cardinality"),(0,n.kt)("th",{parentName:"tr",align:null},"Description"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},"--extract-script:",(0,n.kt)("inlineCode",{parentName:"td"},"<value>")),(0,n.kt)("td",{parentName:"tr",align:null},(0,n.kt)("em",{parentName:"td"},"Optional")),(0,n.kt)("td",{parentName:"tr",align:null})),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},"--domain:",(0,n.kt)("inlineCode",{parentName:"td"},"domain1,domain2 ...")),(0,n.kt)("td",{parentName:"tr",align:null},(0,n.kt)("em",{parentName:"td"},"Optional")),(0,n.kt)("td",{parentName:"tr",align:null},"The domain list for which to generate extract scripts")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},"--template:",(0,n.kt)("inlineCode",{parentName:"td"},"<value>")),(0,n.kt)("td",{parentName:"tr",align:null},(0,n.kt)("em",{parentName:"td"},"Required")),(0,n.kt)("td",{parentName:"tr",align:null},"Script template dir")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},"--audit-schema:",(0,n.kt)("inlineCode",{parentName:"td"},"<value>")),(0,n.kt)("td",{parentName:"tr",align:null},(0,n.kt)("em",{parentName:"td"},"Required")),(0,n.kt)("td",{parentName:"tr",align:null},"Audit DB that will contain the audit export table")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},"--delta-column:",(0,n.kt)("inlineCode",{parentName:"td"},"<value>")),(0,n.kt)("td",{parentName:"tr",align:null},(0,n.kt)("em",{parentName:"td"},"Optional")),(0,n.kt)("td",{parentName:"tr",align:null},"The default date column used to determine new rows to export. Overrides config database-extractor.default-column value.")))))}u.isMDXComponent=!0}}]);