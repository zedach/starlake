"use strict";(self.webpackChunkstarlake_docs=self.webpackChunkstarlake_docs||[]).push([[7938],{3905:(e,t,n)=>{n.d(t,{Zo:()=>u,kt:()=>f});var r=n(7294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function o(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},i=Object.keys(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var s=r.createContext({}),c=function(e){var t=r.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):o(o({},t),e)),n},u=function(e){var t=c(e.components);return r.createElement(s.Provider,{value:t},e.children)},p="mdxType",d={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},h=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,i=e.originalType,s=e.parentName,u=l(e,["components","mdxType","originalType","parentName"]),p=c(n),h=a,f=p["".concat(s,".").concat(h)]||p[h]||d[h]||i;return n?r.createElement(f,o(o({ref:t},u),{},{components:n})):r.createElement(f,o({ref:t},u))}));function f(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var i=n.length,o=new Array(i);o[0]=h;var l={};for(var s in t)hasOwnProperty.call(t,s)&&(l[s]=t[s]);l.originalType=e,l[p]="string"==typeof e?e:a,o[1]=l;for(var c=2;c<i;c++)o[c]=n[c];return r.createElement.apply(null,o)}return r.createElement.apply(null,n)}h.displayName="MDXCreateElement"},4274:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>s,contentTitle:()=>o,default:()=>d,frontMatter:()=>i,metadata:()=>l,toc:()=>c});var r=n(7462),a=(n(7294),n(3905));const i={sidebar_position:50,title:"Scheduling"},o="Scheduling",l={unversionedId:"reference/scheduling",id:"reference/scheduling",title:"Scheduling",description:"Scheduling Spark jobs using the native job schedulers such as Airflow fits the following cases:",source:"@site/docs/reference/50.scheduling.md",sourceDirName:"reference",slug:"/reference/scheduling",permalink:"/starlake/docs/reference/scheduling",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/docs/reference/50.scheduling.md",tags:[],version:"current",sidebarPosition:50,frontMatter:{sidebar_position:50,title:"Scheduling"},sidebar:"starlakeSidebar",previous:{title:"Examples",permalink:"/starlake/docs/reference/examples"},next:{title:"Amazon Web Services EMR",permalink:"/starlake/docs/platform/aws"}},s={},c=[],u={toc:c},p="wrapper";function d(e){let{components:t,...n}=e;return(0,a.kt)(p,(0,r.Z)({},u,n,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("h1",{id:"scheduling"},"Scheduling"),(0,a.kt)("p",null,"Scheduling Spark jobs using the native job schedulers such as Airflow fits the following cases:"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"You are running Starlake wth an explicit reference to the domain and schema you want to ingest.\nIn this case, you need to create as many jobs as the number of schemas")),(0,a.kt)("p",null,"But what if you want to run one job per domain or one job for all the domains and let Starlake detect the\nfile type. In that case, Starlake will run sequentially the ingestion on each file for which a schema is defined."),(0,a.kt)("p",null,"using the property ",(0,a.kt)("inlineCode",{parentName:"p"},"grouped")," or env var ",(0,a.kt)("inlineCode",{parentName:"p"},"SL_GROUP"),", you will only be able to ingest files of the exact same schema simultaneously."),(0,a.kt)("p",null,"To ingest concurrently files with a different schema, Starlake takes advantage of the ",(0,a.kt)("a",{parentName:"p",href:"https://spark.apache.org/docs/latest/job-scheduling.html#scheduling-within-an-application"},"Spark intra-application Scheduler"),".\nJust create in the ",(0,a.kt)("inlineCode",{parentName:"p"},"metadata")," directory, a file named ",(0,a.kt)("inlineCode",{parentName:"p"},"fairscheduler.xml")," and define the pools as described in the Spark documentation and set the following variables:"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"mode: with the value ",(0,a.kt)("inlineCode",{parentName:"li"},"FIFO")," to run the jobs sequentially or ",(0,a.kt)("inlineCode",{parentName:"li"},"FAIR")," to run the jobs concurrently"),(0,a.kt)("li",{parentName:"ul"},"pool-name: set it to the name of the pool assigned to Starlake in the file ",(0,a.kt)("inlineCode",{parentName:"li"},"fairscheduler.xml"),"."),(0,a.kt)("li",{parentName:"ul"},"file (optional): set this value to the absolute path of the file ",(0,a.kt)("inlineCode",{parentName:"li"},"fairscheduler.xml")," if it is not located in the ",(0,a.kt)("inlineCode",{parentName:"li"},"metadata")," folder.\nWhen set to the  empty string, the default job scheduler is used. "),(0,a.kt)("li",{parentName:"ul"},"max-jobs: the number of ingestion to run concurrently with the same Spark Session. The value 1 meaning no concurrency.")),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre"},'scheduling {\n  max-jobs = 10\n  mode = "FAIR"\n  file = "" // empty string means the file fairscheduler.xml is located in the metadata directory \n  pool-name = "starlake-pool"\n}\n')))}d.isMDXComponent=!0}}]);