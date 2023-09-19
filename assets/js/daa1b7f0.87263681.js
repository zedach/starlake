"use strict";(self.webpackChunkstarlake_docs=self.webpackChunkstarlake_docs||[]).push([[9309],{3905:(e,t,n)=>{n.d(t,{Zo:()=>s,kt:()=>k});var a=n(7294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function l(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?l(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):l(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function o(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},l=Object.keys(e);for(a=0;a<l.length;a++)n=l[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(a=0;a<l.length;a++)n=l[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var p=a.createContext({}),d=function(e){var t=a.useContext(p),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},s=function(e){var t=d(e.components);return a.createElement(p.Provider,{value:t},e.children)},c="mdxType",m={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},u=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,l=e.originalType,p=e.parentName,s=o(e,["components","mdxType","originalType","parentName"]),c=d(n),u=r,k=c["".concat(p,".").concat(u)]||c[u]||m[u]||l;return n?a.createElement(k,i(i({ref:t},s),{},{components:n})):a.createElement(k,i({ref:t},s))}));function k(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var l=n.length,i=new Array(l);i[0]=u;var o={};for(var p in t)hasOwnProperty.call(t,p)&&(o[p]=t[p]);o.originalType=e,o[c]="string"==typeof e?e:r,i[1]=o;for(var d=2;d<l;d++)i[d]=n[d];return a.createElement.apply(null,i)}return a.createElement.apply(null,n)}u.displayName="MDXCreateElement"},2921:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>p,contentTitle:()=>i,default:()=>m,frontMatter:()=>l,metadata:()=>o,toc:()=>d});var a=n(7462),r=(n(7294),n(3905));const l={sidebar_position:110,title:"dependencies"},i=void 0,o={unversionedId:"cli/jobs2gv",id:"cli/jobs2gv",title:"dependencies",description:"Synopsis",source:"@site/docs/cli/jobs2gv.md",sourceDirName:"cli",slug:"/cli/jobs2gv",permalink:"/starlake/docs/next/cli/jobs2gv",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/docs/cli/jobs2gv.md",tags:[],version:"current",sidebarPosition:110,frontMatter:{sidebar_position:110,title:"dependencies"},sidebar:"starlakeSidebar",previous:{title:"infer-schema",permalink:"/starlake/docs/next/cli/infer-schema"},next:{title:"kafkaload",permalink:"/starlake/docs/next/cli/kafkaload"}},p={},d=[{value:"Synopsis",id:"synopsis",level:2},{value:"Description",id:"description",level:2},{value:"Parameters",id:"parameters",level:2}],s={toc:d},c="wrapper";function m(e){let{components:t,...n}=e;return(0,r.kt)(c,(0,a.Z)({},s,n,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h2",{id:"synopsis"},"Synopsis"),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"starlake dependencies ","[options]")),(0,r.kt)("h2",{id:"description"},"Description"),(0,r.kt)("p",null,"Generate Task dependencies graph"),(0,r.kt)("h2",{id:"parameters"},"Parameters"),(0,r.kt)("table",null,(0,r.kt)("thead",{parentName:"table"},(0,r.kt)("tr",{parentName:"thead"},(0,r.kt)("th",{parentName:"tr",align:null},"Parameter"),(0,r.kt)("th",{parentName:"tr",align:null},"Cardinality"),(0,r.kt)("th",{parentName:"tr",align:null},"Description"))),(0,r.kt)("tbody",{parentName:"table"},(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--output-dir:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"Where to save the generated dot file ? Output to the console by default")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--task:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"Compute dependencies of this job only. If not specified, compute all jobs.")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--reload:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"Should we reload the domains first ?")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--verbose:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"Should we generate one graph per job ?")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--viz:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"Should we generate one graph per job ?")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--print:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"Print dependencies as text")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--objects:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"comma separated list of objects to display: task, table, view, unknown")))))}m.isMDXComponent=!0}}]);