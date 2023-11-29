"use strict";(self.webpackChunkstarlake=self.webpackChunkstarlake||[]).push([[7190],{3905:(e,t,n)=>{n.d(t,{Zo:()=>m,kt:()=>k});var a=n(67294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function l(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?l(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):l(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function o(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},l=Object.keys(e);for(a=0;a<l.length;a++)n=l[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(a=0;a<l.length;a++)n=l[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var s=a.createContext({}),p=function(e){var t=a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},m=function(e){var t=p(e.components);return a.createElement(s.Provider,{value:t},e.children)},d="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},c=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,l=e.originalType,s=e.parentName,m=o(e,["components","mdxType","originalType","parentName"]),d=p(n),c=r,k=d["".concat(s,".").concat(c)]||d[c]||u[c]||l;return n?a.createElement(k,i(i({ref:t},m),{},{components:n})):a.createElement(k,i({ref:t},m))}));function k(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var l=n.length,i=new Array(l);i[0]=c;var o={};for(var s in t)hasOwnProperty.call(t,s)&&(o[s]=t[s]);o.originalType=e,o[d]="string"==typeof e?e:r,i[1]=o;for(var p=2;p<l;p++)i[p]=n[p];return a.createElement.apply(null,i)}return a.createElement.apply(null,n)}c.displayName="MDXCreateElement"},10381:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>s,contentTitle:()=>i,default:()=>u,frontMatter:()=>l,metadata:()=>o,toc:()=>p});var a=n(87462),r=(n(67294),n(3905));const l={sidebar_position:10},i="Environment",o={unversionedId:"configuration/environment",id:"configuration/environment",title:"Environment",description:"Env specific variables",source:"@site/docs/0850-configuration/0100.environment.mdx",sourceDirName:"0850-configuration",slug:"/configuration/environment",permalink:"/starlake/docs/next/configuration/environment",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/docs/0850-configuration/0100.environment.mdx",tags:[],version:"current",sidebarPosition:10,frontMatter:{sidebar_position:10},sidebar:"starlakeSidebar",previous:{title:"Configuration",permalink:"/starlake/docs/next/category/configuration"},next:{title:"Extract",permalink:"/starlake/docs/next/configuration/extract"}},s={},p=[{value:"Env specific variables",id:"env-specific-variables",level:2},{value:"Global Variables",id:"global-variables",level:2},{value:"Preset variables",id:"preset-variables",level:2}],m={toc:p},d="wrapper";function u(e){let{components:t,...n}=e;return(0,r.kt)(d,(0,a.Z)({},m,n,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"environment"},"Environment"),(0,r.kt)("h2",{id:"env-specific-variables"},"Env specific variables"),(0,r.kt)("p",null,"Starlake allows you to use variables almost everywhere in the domain and job files.\nFor example, you may need to set the folder name to watch to a different value\nin development and production environments. This is where variables may help. They are enclosed inside\n",(0,r.kt)("inlineCode",{parentName:"p"},"${}")," or ",(0,r.kt)("inlineCode",{parentName:"p"},"{{}}")),(0,r.kt)("p",null,"Assuming we have a ",(0,r.kt)("inlineCode",{parentName:"p"},"sales")," domain as follows:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'load:\n  name: "sales"\n  directory: "{{root_path}}/sales"\n  ack: "ack"\n')),(0,r.kt)("p",null,"We create a file ",(0,r.kt)("inlineCode",{parentName:"p"},"env.DEV.sl.yml")," in the metadata folder "),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'env:\n  root_path: "/tmp/userguide"\n')),(0,r.kt)("p",null,"and the file ",(0,r.kt)("inlineCode",{parentName:"p"},"env.PROD.sl.yml")," in the metadata folder"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},'env:\n  root_path: "/cluster/userguide"\n')),(0,r.kt)("p",null,"To apply the substitution in the DEV env set the SL_ENV variable before running Starlake as follows:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-shell"},"export SL_ENV=DEV\n")),(0,r.kt)("p",null,"In Production set it rather to:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-shell"},"export SL_ENV=PROD\n")),(0,r.kt)("h2",{id:"global-variables"},"Global Variables"),(0,r.kt)("p",null,"To define variables across environments, declare them in the ",(0,r.kt)("inlineCode",{parentName:"p"},"env.sl.yml")," file in the ",(0,r.kt)("inlineCode",{parentName:"p"},"metadata")," folder."),(0,r.kt)("p",null,"Global variables definitions may be superseded by the env specific variables files.  "),(0,r.kt)("h2",{id:"preset-variables"},"Preset variables"),(0,r.kt)("p",null,"The following variables are predefined and may be used anywhere:"),(0,r.kt)("table",null,(0,r.kt)("thead",{parentName:"table"},(0,r.kt)("tr",{parentName:"thead"},(0,r.kt)("th",{parentName:"tr",align:null},"Date Time Variable"),(0,r.kt)("th",{parentName:"tr",align:null},"Format"))),(0,r.kt)("tbody",{parentName:"table"},(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"sl_date"),(0,r.kt)("td",{parentName:"tr",align:null},"yyyyMMdd")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"sl_datetime"),(0,r.kt)("td",{parentName:"tr",align:null},"yyyyMMddHHmmss")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"sl_year"),(0,r.kt)("td",{parentName:"tr",align:null},"yy")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"sl_month"),(0,r.kt)("td",{parentName:"tr",align:null},"MM")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"sl_day"),(0,r.kt)("td",{parentName:"tr",align:null},"dd")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"sl_hour"),(0,r.kt)("td",{parentName:"tr",align:null},"HH")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"sl_minute"),(0,r.kt)("td",{parentName:"tr",align:null},"mm")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"sl_second"),(0,r.kt)("td",{parentName:"tr",align:null},"ss")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"sl_milli"),(0,r.kt)("td",{parentName:"tr",align:null},"SSS")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"sl_epoch_second"),(0,r.kt)("td",{parentName:"tr",align:null},"Number of seconds since 1/1/1970")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"sl_epoch_milli"),(0,r.kt)("td",{parentName:"tr",align:null},"Number of milliseconds since 1/1/1970")))))}u.isMDXComponent=!0}}]);