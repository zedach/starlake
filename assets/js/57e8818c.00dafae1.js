"use strict";(self.webpackChunkstarlake=self.webpackChunkstarlake||[]).push([[595],{3905:(e,t,r)=>{r.d(t,{Zo:()=>m,kt:()=>y});var n=r(67294);function a(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function l(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function i(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?l(Object(r),!0).forEach((function(t){a(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):l(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function o(e,t){if(null==e)return{};var r,n,a=function(e,t){if(null==e)return{};var r,n,a={},l=Object.keys(e);for(n=0;n<l.length;n++)r=l[n],t.indexOf(r)>=0||(a[r]=e[r]);return a}(e,t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(n=0;n<l.length;n++)r=l[n],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(a[r]=e[r])}return a}var p=n.createContext({}),s=function(e){var t=n.useContext(p),r=t;return e&&(r="function"==typeof e?e(t):i(i({},t),e)),r},m=function(e){var t=s(e.components);return n.createElement(p.Provider,{value:t},e.children)},d="mdxType",c={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},u=n.forwardRef((function(e,t){var r=e.components,a=e.mdxType,l=e.originalType,p=e.parentName,m=o(e,["components","mdxType","originalType","parentName"]),d=s(r),u=a,y=d["".concat(p,".").concat(u)]||d[u]||c[u]||l;return r?n.createElement(y,i(i({ref:t},m),{},{components:r})):n.createElement(y,i({ref:t},m))}));function y(e,t){var r=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var l=r.length,i=new Array(l);i[0]=u;var o={};for(var p in t)hasOwnProperty.call(t,p)&&(o[p]=t[p]);o.originalType=e,o[d]="string"==typeof e?e:a,i[1]=o;for(var s=2;s<l;s++)i[s]=r[s];return n.createElement.apply(null,i)}return n.createElement.apply(null,r)}u.displayName="MDXCreateElement"},12722:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>p,contentTitle:()=>i,default:()=>c,frontMatter:()=>l,metadata:()=>o,toc:()=>s});var n=r(87462),a=(r(67294),r(3905));const l={sidebar_position:300,title:"xls2yml"},i=void 0,o={unversionedId:"cli/xls2ymljob",id:"cli/xls2ymljob",title:"xls2yml",description:"Synopsis",source:"@site/docs/0800-cli/xls2ymljob.md",sourceDirName:"0800-cli",slug:"/cli/xls2ymljob",permalink:"/starlake/docs/next/cli/xls2ymljob",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/docs/0800-cli/xls2ymljob.md",tags:[],version:"current",sidebarPosition:300,frontMatter:{sidebar_position:300,title:"xls2yml"},sidebar:"starlakeSidebar",previous:{title:"xls2yml",permalink:"/starlake/docs/next/cli/xls2yml"},next:{title:"yml2ddl",permalink:"/starlake/docs/next/cli/yml2ddl"}},p={},s=[{value:"Synopsis",id:"synopsis",level:2},{value:"Description",id:"description",level:2},{value:"Parameters",id:"parameters",level:2}],m={toc:s},d="wrapper";function c(e){let{components:t,...r}=e;return(0,a.kt)(d,(0,n.Z)({},m,r,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("h2",{id:"synopsis"},"Synopsis"),(0,a.kt)("p",null,(0,a.kt)("strong",{parentName:"p"},"starlake xls2yml ","[options]")),(0,a.kt)("h2",{id:"description"},"Description"),(0,a.kt)("h2",{id:"parameters"},"Parameters"),(0,a.kt)("table",null,(0,a.kt)("thead",{parentName:"table"},(0,a.kt)("tr",{parentName:"thead"},(0,a.kt)("th",{parentName:"tr",align:null},"Parameter"),(0,a.kt)("th",{parentName:"tr",align:null},"Cardinality"),(0,a.kt)("th",{parentName:"tr",align:null},"Description"))),(0,a.kt)("tbody",{parentName:"table"},(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--files:",(0,a.kt)("inlineCode",{parentName:"td"},"<value>")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Required")),(0,a.kt)("td",{parentName:"tr",align:null},"List of Excel files describing domains & schemas or jobs")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--iamPolicyTagsFile:",(0,a.kt)("inlineCode",{parentName:"td"},"<value>")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Optional")),(0,a.kt)("td",{parentName:"tr",align:null},"If true generate IAM PolicyTags YML")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--outputDir:",(0,a.kt)("inlineCode",{parentName:"td"},"<value>")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Optional")),(0,a.kt)("td",{parentName:"tr",align:null},"Path for saving the resulting YAML file(s). Starlake domains path is used by default.")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--policyFile:",(0,a.kt)("inlineCode",{parentName:"td"},"<value>")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Optional")),(0,a.kt)("td",{parentName:"tr",align:null},"Optional File for centralising ACL & RLS definition.")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--job:",(0,a.kt)("inlineCode",{parentName:"td"},"<value>")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Optional")),(0,a.kt)("td",{parentName:"tr",align:null},"If true generate YML for a Job.")))))}c.isMDXComponent=!0}}]);