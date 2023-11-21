"use strict";(self.webpackChunkstarlake_docs=self.webpackChunkstarlake_docs||[]).push([[3685],{3905:(t,e,n)=>{n.d(e,{Zo:()=>p,kt:()=>k});var r=n(67294);function a(t,e,n){return e in t?Object.defineProperty(t,e,{value:n,enumerable:!0,configurable:!0,writable:!0}):t[e]=n,t}function i(t,e){var n=Object.keys(t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(t);e&&(r=r.filter((function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable}))),n.push.apply(n,r)}return n}function l(t){for(var e=1;e<arguments.length;e++){var n=null!=arguments[e]?arguments[e]:{};e%2?i(Object(n),!0).forEach((function(e){a(t,e,n[e])})):Object.getOwnPropertyDescriptors?Object.defineProperties(t,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(e){Object.defineProperty(t,e,Object.getOwnPropertyDescriptor(n,e))}))}return t}function o(t,e){if(null==t)return{};var n,r,a=function(t,e){if(null==t)return{};var n,r,a={},i=Object.keys(t);for(r=0;r<i.length;r++)n=i[r],e.indexOf(n)>=0||(a[n]=t[n]);return a}(t,e);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(t);for(r=0;r<i.length;r++)n=i[r],e.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(t,n)&&(a[n]=t[n])}return a}var s=r.createContext({}),c=function(t){var e=r.useContext(s),n=e;return t&&(n="function"==typeof t?t(e):l(l({},e),t)),n},p=function(t){var e=c(t.components);return r.createElement(s.Provider,{value:e},t.children)},d="mdxType",m={inlineCode:"code",wrapper:function(t){var e=t.children;return r.createElement(r.Fragment,{},e)}},u=r.forwardRef((function(t,e){var n=t.components,a=t.mdxType,i=t.originalType,s=t.parentName,p=o(t,["components","mdxType","originalType","parentName"]),d=c(n),u=a,k=d["".concat(s,".").concat(u)]||d[u]||m[u]||i;return n?r.createElement(k,l(l({ref:e},p),{},{components:n})):r.createElement(k,l({ref:e},p))}));function k(t,e){var n=arguments,a=e&&e.mdxType;if("string"==typeof t||a){var i=n.length,l=new Array(i);l[0]=u;var o={};for(var s in e)hasOwnProperty.call(e,s)&&(o[s]=e[s]);o.originalType=t,o[d]="string"==typeof t?t:a,l[1]=o;for(var c=2;c<i;c++)l[c]=n[c];return r.createElement.apply(null,l)}return r.createElement.apply(null,n)}u.displayName="MDXCreateElement"},3211:(t,e,n)=>{n.r(e),n.d(e,{assets:()=>s,contentTitle:()=>l,default:()=>m,frontMatter:()=>i,metadata:()=>o,toc:()=>c});var r=n(87462),a=(n(67294),n(3905));const i={sidebar_position:190,title:"watch"},l=void 0,o={unversionedId:"cli/watch",id:"version-0.8.0/cli/watch",title:"watch",description:"Synopsis",source:"@site/versioned_docs/version-0.8.0/cli/watch.md",sourceDirName:"cli",slug:"/cli/watch",permalink:"/starlake/docs/0.8.0/cli/watch",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/versioned_docs/version-0.8.0/cli/watch.md",tags:[],version:"0.8.0",sidebarPosition:190,frontMatter:{sidebar_position:190,title:"watch"},sidebar:"starlakeSidebar",previous:{title:"validate",permalink:"/starlake/docs/0.8.0/cli/validate"},next:{title:"xls2yml",permalink:"/starlake/docs/0.8.0/cli/xls2yml"}},s={},c=[{value:"Synopsis",id:"synopsis",level:2},{value:"Description",id:"description",level:2},{value:"Parameters",id:"parameters",level:2}],p={toc:c},d="wrapper";function m(t){let{components:e,...n}=t;return(0,a.kt)(d,(0,r.Z)({},p,n,{components:e,mdxType:"MDXLayout"}),(0,a.kt)("h2",{id:"synopsis"},"Synopsis"),(0,a.kt)("p",null,(0,a.kt)("strong",{parentName:"p"},"starlake watch ","[options]")),(0,a.kt)("h2",{id:"description"},"Description"),(0,a.kt)("h2",{id:"parameters"},"Parameters"),(0,a.kt)("table",null,(0,a.kt)("thead",{parentName:"table"},(0,a.kt)("tr",{parentName:"thead"},(0,a.kt)("th",{parentName:"tr",align:null},"Parameter"),(0,a.kt)("th",{parentName:"tr",align:null},"Cardinality"),(0,a.kt)("th",{parentName:"tr",align:null},"Description"))),(0,a.kt)("tbody",{parentName:"table"},(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--include:",(0,a.kt)("inlineCode",{parentName:"td"},"domain1,domain2...")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Optional")),(0,a.kt)("td",{parentName:"tr",align:null},"Domains to watch")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--exclude:",(0,a.kt)("inlineCode",{parentName:"td"},"domain1,domain2...")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Optional")),(0,a.kt)("td",{parentName:"tr",align:null},"Domains not to watch")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--schemas:",(0,a.kt)("inlineCode",{parentName:"td"},"schema1,schema2,schema3...")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Optional")),(0,a.kt)("td",{parentName:"tr",align:null},"Schemas to watch")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},"--options:",(0,a.kt)("inlineCode",{parentName:"td"},"k1=v1,k2=v2...")),(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Optional")),(0,a.kt)("td",{parentName:"tr",align:null},"Watch arguments to be used as substitutions")))))}m.isMDXComponent=!0}}]);