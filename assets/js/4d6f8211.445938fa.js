"use strict";(self.webpackChunkstarlake_docs=self.webpackChunkstarlake_docs||[]).push([[9142],{3905:(t,e,a)=>{a.d(e,{Zo:()=>c,kt:()=>k});var r=a(7294);function n(t,e,a){return e in t?Object.defineProperty(t,e,{value:a,enumerable:!0,configurable:!0,writable:!0}):t[e]=a,t}function l(t,e){var a=Object.keys(t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(t);e&&(r=r.filter((function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable}))),a.push.apply(a,r)}return a}function i(t){for(var e=1;e<arguments.length;e++){var a=null!=arguments[e]?arguments[e]:{};e%2?l(Object(a),!0).forEach((function(e){n(t,e,a[e])})):Object.getOwnPropertyDescriptors?Object.defineProperties(t,Object.getOwnPropertyDescriptors(a)):l(Object(a)).forEach((function(e){Object.defineProperty(t,e,Object.getOwnPropertyDescriptor(a,e))}))}return t}function o(t,e){if(null==t)return{};var a,r,n=function(t,e){if(null==t)return{};var a,r,n={},l=Object.keys(t);for(r=0;r<l.length;r++)a=l[r],e.indexOf(a)>=0||(n[a]=t[a]);return n}(t,e);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(t);for(r=0;r<l.length;r++)a=l[r],e.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(t,a)&&(n[a]=t[a])}return n}var p=r.createContext({}),s=function(t){var e=r.useContext(p),a=e;return t&&(a="function"==typeof t?t(e):i(i({},e),t)),a},c=function(t){var e=s(t.components);return r.createElement(p.Provider,{value:e},t.children)},m="mdxType",u={inlineCode:"code",wrapper:function(t){var e=t.children;return r.createElement(r.Fragment,{},e)}},d=r.forwardRef((function(t,e){var a=t.components,n=t.mdxType,l=t.originalType,p=t.parentName,c=o(t,["components","mdxType","originalType","parentName"]),m=s(a),d=n,k=m["".concat(p,".").concat(d)]||m[d]||u[d]||l;return a?r.createElement(k,i(i({ref:e},c),{},{components:a})):r.createElement(k,i({ref:e},c))}));function k(t,e){var a=arguments,n=e&&e.mdxType;if("string"==typeof t||n){var l=a.length,i=new Array(l);i[0]=d;var o={};for(var p in e)hasOwnProperty.call(e,p)&&(o[p]=e[p]);o.originalType=t,o[m]="string"==typeof t?t:n,i[1]=o;for(var s=2;s<l;s++)i[s]=a[s];return r.createElement.apply(null,i)}return r.createElement.apply(null,a)}d.displayName="MDXCreateElement"},1514:(t,e,a)=>{a.r(e),a.d(e,{assets:()=>p,contentTitle:()=>i,default:()=>u,frontMatter:()=>l,metadata:()=>o,toc:()=>s});var r=a(7462),n=(a(7294),a(3905));const l={sidebar_position:80,title:"extract-script"},i=void 0,o={unversionedId:"cli/extract-script",id:"cli/extract-script",title:"extract-script",description:"Synopsis",source:"@site/docs/cli/extract-script.md",sourceDirName:"cli",slug:"/cli/extract-script",permalink:"/starlake/docs/next/cli/extract-script",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/docs/cli/extract-script.md",tags:[],version:"current",sidebarPosition:80,frontMatter:{sidebar_position:80,title:"extract-script"},sidebar:"starlakeSidebar",previous:{title:"extract-schema",permalink:"/starlake/docs/next/cli/extract-schema"},next:{title:"import",permalink:"/starlake/docs/next/cli/import"}},p={},s=[{value:"Synopsis",id:"synopsis",level:2},{value:"Description",id:"description",level:2},{value:"Parameters",id:"parameters",level:2}],c={toc:s},m="wrapper";function u(t){let{components:e,...a}=t;return(0,n.kt)(m,(0,r.Z)({},c,a,{components:e,mdxType:"MDXLayout"}),(0,n.kt)("h2",{id:"synopsis"},"Synopsis"),(0,n.kt)("p",null,(0,n.kt)("strong",{parentName:"p"},"starlake extract-script ","[options]")),(0,n.kt)("h2",{id:"description"},"Description"),(0,n.kt)("p",null,"For domain extraction, the schemas should at least, specify :"),(0,n.kt)("ul",null,(0,n.kt)("li",{parentName:"ul"},"a table name (schemas.name)"),(0,n.kt)("li",{parentName:"ul"},"a file pattern (schemas.pattern) which is used as the export file base name"),(0,n.kt)("li",{parentName:"ul"},"a write mode (schemas.metadata.write): APPEND or OVERWRITE"),(0,n.kt)("li",{parentName:"ul"},"a delta column (schemas.merge.timestamp) if in APPEND mode : the default column which is used to determine new rows for each exports"),(0,n.kt)("li",{parentName:"ul"},"the columns to extract (schemas.attributes.name*)")),(0,n.kt)("p",null,"You also have to provide a Mustache (",(0,n.kt)("a",{parentName:"p",href:"http://mustache.github.io/mustache.5.html"},"http://mustache.github.io/mustache.5.html"),") template file."),(0,n.kt)("p",null,"In there you'll write your extraction export process (sqlplus for Oracle, pgsql for PostgreSQL as an example).\nIn that template you can use the following parameters:"),(0,n.kt)("ul",null,(0,n.kt)("li",{parentName:"ul"},"table_name  -> the table to export"),(0,n.kt)("li",{parentName:"ul"},"delimiter   -> the resulting dsv file delimiter"),(0,n.kt)("li",{parentName:"ul"},"columns     -> the columns to export\ncolumns is a Mustache map, it gives you access, for each column, to:",(0,n.kt)("ul",{parentName:"li"},(0,n.kt)("li",{parentName:"ul"},"name               -> the column name"),(0,n.kt)("li",{parentName:"ul"},'trailing_col_char  -> the separator to append to the column (, if there are more columns to come, "" otherwise)\nHere is an example how to use it in a template:')))),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-sql"},"                              SELECT\n                              {{#columns}}\n                              TO_CHAR({{name}}){{trailing_col_char}}\n                              {{/columns}}\n                              FROM\n                              {{table_name}};\n")),(0,n.kt)("p",null," full_export -> if the export is a full or delta export (the logic is to be implemented in your script)"),(0,n.kt)("h2",{id:"parameters"},"Parameters"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:null},"Parameter"),(0,n.kt)("th",{parentName:"tr",align:null},"Cardinality"),(0,n.kt)("th",{parentName:"tr",align:null},"Description"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},"--extract-script:",(0,n.kt)("inlineCode",{parentName:"td"},"<value>")),(0,n.kt)("td",{parentName:"tr",align:null},(0,n.kt)("em",{parentName:"td"},"Optional")),(0,n.kt)("td",{parentName:"tr",align:null})),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},"--domain:",(0,n.kt)("inlineCode",{parentName:"td"},"domain1,domain2 ...")),(0,n.kt)("td",{parentName:"tr",align:null},(0,n.kt)("em",{parentName:"td"},"Optional")),(0,n.kt)("td",{parentName:"tr",align:null},"The domain list for which to generate extract scripts")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},"--template:",(0,n.kt)("inlineCode",{parentName:"td"},"<value>")),(0,n.kt)("td",{parentName:"tr",align:null},(0,n.kt)("em",{parentName:"td"},"Required")),(0,n.kt)("td",{parentName:"tr",align:null},"Script template dir")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},"--audit-schema:",(0,n.kt)("inlineCode",{parentName:"td"},"<value>")),(0,n.kt)("td",{parentName:"tr",align:null},(0,n.kt)("em",{parentName:"td"},"Required")),(0,n.kt)("td",{parentName:"tr",align:null},"Audit DB that will contain the audit export table")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:null},"--delta-column:",(0,n.kt)("inlineCode",{parentName:"td"},"<value>")),(0,n.kt)("td",{parentName:"tr",align:null},(0,n.kt)("em",{parentName:"td"},"Optional")),(0,n.kt)("td",{parentName:"tr",align:null},"The default date column used to determine new rows to export. Overrides config database-extractor.defaultColumn value.")))))}u.isMDXComponent=!0}}]);