"use strict";(self.webpackChunkstarlake_docs=self.webpackChunkstarlake_docs||[]).push([[3459],{3905:(e,t,n)=>{n.d(t,{Zo:()=>m,kt:()=>k});var r=n(7294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function l(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?l(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):l(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function o(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},l=Object.keys(e);for(r=0;r<l.length;r++)n=l[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(r=0;r<l.length;r++)n=l[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var p=r.createContext({}),d=function(e){var t=r.useContext(p),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},m=function(e){var t=d(e.components);return r.createElement(p.Provider,{value:t},e.children)},s="mdxType",c={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},u=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,l=e.originalType,p=e.parentName,m=o(e,["components","mdxType","originalType","parentName"]),s=d(n),u=a,k=s["".concat(p,".").concat(u)]||s[u]||c[u]||l;return n?r.createElement(k,i(i({ref:t},m),{},{components:n})):r.createElement(k,i({ref:t},m))}));function k(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var l=n.length,i=new Array(l);i[0]=u;var o={};for(var p in t)hasOwnProperty.call(t,p)&&(o[p]=t[p]);o.originalType=e,o[s]="string"==typeof e?e:a,i[1]=o;for(var d=2;d<l;d++)i[d]=n[d];return r.createElement.apply(null,i)}return r.createElement.apply(null,n)}u.displayName="MDXCreateElement"},496:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>p,contentTitle:()=>i,default:()=>c,frontMatter:()=>l,metadata:()=>o,toc:()=>d});var r=n(7462),a=(n(7294),n(3905));const l={sidebar_position:100,title:"infer-schema"},i=void 0,o={unversionedId:"cli/infer-schema",id:"cli/infer-schema",title:"infer-schema",description:"Synopsis",source:"@site/docs/cli/infer-schema.md",sourceDirName:"cli",slug:"/cli/infer-schema",permalink:"/starlake/docs/next/cli/infer-schema",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/docs/cli/infer-schema.md",tags:[],version:"current",sidebarPosition:100,frontMatter:{sidebar_position:100,title:"infer-schema"},sidebar:"starlakeSidebar",previous:{title:"yml2ddl",permalink:"/starlake/docs/next/cli/infer-ddl"},next:{title:"dependencies",permalink:"/starlake/docs/next/cli/jobs2gv"}},p={},d=[{value:"Synopsis",id:"synopsis",level:2},{value:"Description",id:"description",level:2},{value:"Parameters",id:"parameters",level:2}],m={toc:d},s="wrapper";function c(e){let{components:t,...n}=e;return(0,a.kt)(s,(0,r.Z)({},m,n,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("h2",{id:"synopsis"},"Synopsis"),(0,a.kt)("p",null,(0,a.kt)("strong",{parentName:"p"},"starlake infer-schema ","[options]")),(0,a.kt)("h2",{id:"description"},"Description"),(0,a.kt)("h2",{id:"parameters"},"Parameters"),(0,a.kt)("table",null,(0,a.kt)("thead",{parentName:"table"},(0,a.kt)("tr",{parentName:"thead"},(0,a.kt)("th",{parentName:"tr",align:null},"Parameter"),(0,a.kt)("th",{parentName:"tr",align:null},"Cardinality"),(0,a.kt)("th",{parentName:"tr",align:null},"Description"))),(0,a.kt)("tbody",{parentName:"table"},(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--domain:",(0,a.kt)("inlineCode",{parentName:"td"},"<value>")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Required")),(0,a.kt)("td",{parentName:"tr",align:null},"Domain Name")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--table:",(0,a.kt)("inlineCode",{parentName:"td"},"<value>")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Required")),(0,a.kt)("td",{parentName:"tr",align:null},"Table Name")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--input:",(0,a.kt)("inlineCode",{parentName:"td"},"<value>")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Required")),(0,a.kt)("td",{parentName:"tr",align:null},"Dataset Input Path")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--output-dir:",(0,a.kt)("inlineCode",{parentName:"td"},"<value>")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Optional")),(0,a.kt)("td",{parentName:"tr",align:null},"Domain YAML Output Path")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--format:",(0,a.kt)("inlineCode",{parentName:"td"},"<value>")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Optional")),(0,a.kt)("td",{parentName:"tr",align:null},"Force input file format")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--with-header:",(0,a.kt)("inlineCode",{parentName:"td"},"<value>")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Optional")),(0,a.kt)("td",{parentName:"tr",align:null},"Does the file contain a header (For CSV files only)")))))}c.isMDXComponent=!0}}]);