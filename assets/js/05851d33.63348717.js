"use strict";(self.webpackChunkstarlake_docs=self.webpackChunkstarlake_docs||[]).push([[3711],{3905:(t,e,a)=>{a.d(e,{Zo:()=>s,kt:()=>k});var n=a(67294);function r(t,e,a){return e in t?Object.defineProperty(t,e,{value:a,enumerable:!0,configurable:!0,writable:!0}):t[e]=a,t}function l(t,e){var a=Object.keys(t);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(t);e&&(n=n.filter((function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable}))),a.push.apply(a,n)}return a}function i(t){for(var e=1;e<arguments.length;e++){var a=null!=arguments[e]?arguments[e]:{};e%2?l(Object(a),!0).forEach((function(e){r(t,e,a[e])})):Object.getOwnPropertyDescriptors?Object.defineProperties(t,Object.getOwnPropertyDescriptors(a)):l(Object(a)).forEach((function(e){Object.defineProperty(t,e,Object.getOwnPropertyDescriptor(a,e))}))}return t}function o(t,e){if(null==t)return{};var a,n,r=function(t,e){if(null==t)return{};var a,n,r={},l=Object.keys(t);for(n=0;n<l.length;n++)a=l[n],e.indexOf(a)>=0||(r[a]=t[a]);return r}(t,e);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(t);for(n=0;n<l.length;n++)a=l[n],e.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(t,a)&&(r[a]=t[a])}return r}var p=n.createContext({}),d=function(t){var e=n.useContext(p),a=e;return t&&(a="function"==typeof t?t(e):i(i({},e),t)),a},s=function(t){var e=d(t.components);return n.createElement(p.Provider,{value:e},t.children)},m="mdxType",c={inlineCode:"code",wrapper:function(t){var e=t.children;return n.createElement(n.Fragment,{},e)}},u=n.forwardRef((function(t,e){var a=t.components,r=t.mdxType,l=t.originalType,p=t.parentName,s=o(t,["components","mdxType","originalType","parentName"]),m=d(a),u=r,k=m["".concat(p,".").concat(u)]||m[u]||c[u]||l;return a?n.createElement(k,i(i({ref:e},s),{},{components:a})):n.createElement(k,i({ref:e},s))}));function k(t,e){var a=arguments,r=e&&e.mdxType;if("string"==typeof t||r){var l=a.length,i=new Array(l);i[0]=u;var o={};for(var p in e)hasOwnProperty.call(e,p)&&(o[p]=e[p]);o.originalType=t,o[m]="string"==typeof t?t:r,i[1]=o;for(var d=2;d<l;d++)i[d]=a[d];return n.createElement.apply(null,i)}return n.createElement.apply(null,a)}u.displayName="MDXCreateElement"},32215:(t,e,a)=>{a.r(e),a.d(e,{assets:()=>p,contentTitle:()=>i,default:()=>c,frontMatter:()=>l,metadata:()=>o,toc:()=>d});var n=a(87462),r=(a(67294),a(3905));const l={sidebar_position:60,title:"esload"},i=void 0,o={unversionedId:"cli/esload",id:"version-1.0.0/cli/esload",title:"esload",description:"Synopsis",source:"@site/versioned_docs/version-1.0.0/cli/esload.md",sourceDirName:"cli",slug:"/cli/esload",permalink:"/starlake/docs/cli/esload",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/versioned_docs/version-1.0.0/cli/esload.md",tags:[],version:"1.0.0",sidebarPosition:60,frontMatter:{sidebar_position:60,title:"esload"},sidebar:"starlakeSidebar",previous:{title:"cnxload",permalink:"/starlake/docs/cli/cnxload"},next:{title:"extract-data",permalink:"/starlake/docs/cli/extract-data"}},p={},d=[{value:"Synopsis",id:"synopsis",level:2},{value:"Description",id:"description",level:2},{value:"Parameters",id:"parameters",level:2}],s={toc:d},m="wrapper";function c(t){let{components:e,...a}=t;return(0,r.kt)(m,(0,n.Z)({},s,a,{components:e,mdxType:"MDXLayout"}),(0,r.kt)("h2",{id:"synopsis"},"Synopsis"),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"starlake esload ","[options]")),(0,r.kt)("h2",{id:"description"},"Description"),(0,r.kt)("h2",{id:"parameters"},"Parameters"),(0,r.kt)("table",null,(0,r.kt)("thead",{parentName:"table"},(0,r.kt)("tr",{parentName:"thead"},(0,r.kt)("th",{parentName:"tr",align:null},"Parameter"),(0,r.kt)("th",{parentName:"tr",align:null},"Cardinality"),(0,r.kt)("th",{parentName:"tr",align:null},"Description"))),(0,r.kt)("tbody",{parentName:"table"},(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--timestamp:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"Elasticsearch index timestamp suffix as in {@timestamp")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--id:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"Elasticsearch Document Id")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--mapping:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"Path to Elasticsearch Mapping File")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--domain:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Required")),(0,r.kt)("td",{parentName:"tr",align:null},"Domain Name")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--schema:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Required")),(0,r.kt)("td",{parentName:"tr",align:null},"Schema Name")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--format:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Required")),(0,r.kt)("td",{parentName:"tr",align:null},"Dataset input file : parquet, json or json-array")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--dataset:",(0,r.kt)("inlineCode",{parentName:"td"},"<value>")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"Input dataset path")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"--conf:",(0,r.kt)("inlineCode",{parentName:"td"},"es.batch.size.entries=1000, es.batch.size.bytes=1mb...")),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Optional")),(0,r.kt)("td",{parentName:"tr",align:null},"esSpark configuration options. See ",(0,r.kt)("a",{parentName:"td",href:"https://www.elastic.co/guide/en/elasticsearch/hadoop/current/configuration.html"},"https://www.elastic.co/guide/en/elasticsearch/hadoop/current/configuration.html"))))))}c.isMDXComponent=!0}}]);