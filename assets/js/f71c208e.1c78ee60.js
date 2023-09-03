"use strict";(self.webpackChunkstarlake_docs=self.webpackChunkstarlake_docs||[]).push([[5992],{3905:(e,t,r)=>{r.d(t,{Zo:()=>c,kt:()=>f});var a=r(7294);function n(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function l(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,a)}return r}function o(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?l(Object(r),!0).forEach((function(t){n(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):l(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function s(e,t){if(null==e)return{};var r,a,n=function(e,t){if(null==e)return{};var r,a,n={},l=Object.keys(e);for(a=0;a<l.length;a++)r=l[a],t.indexOf(r)>=0||(n[r]=e[r]);return n}(e,t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(a=0;a<l.length;a++)r=l[a],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(n[r]=e[r])}return n}var i=a.createContext({}),u=function(e){var t=a.useContext(i),r=t;return e&&(r="function"==typeof e?e(t):o(o({},t),e)),r},c=function(e){var t=u(e.components);return a.createElement(i.Provider,{value:t},e.children)},p="mdxType",m={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},d=a.forwardRef((function(e,t){var r=e.components,n=e.mdxType,l=e.originalType,i=e.parentName,c=s(e,["components","mdxType","originalType","parentName"]),p=u(r),d=n,f=p["".concat(i,".").concat(d)]||p[d]||m[d]||l;return r?a.createElement(f,o(o({ref:t},c),{},{components:r})):a.createElement(f,o({ref:t},c))}));function f(e,t){var r=arguments,n=t&&t.mdxType;if("string"==typeof e||n){var l=r.length,o=new Array(l);o[0]=d;var s={};for(var i in t)hasOwnProperty.call(t,i)&&(s[i]=t[i]);s.originalType=e,s[p]="string"==typeof e?e:n,o[1]=s;for(var u=2;u<l;u++)o[u]=r[u];return a.createElement.apply(null,o)}return a.createElement.apply(null,r)}d.displayName="MDXCreateElement"},5162:(e,t,r)=>{r.d(t,{Z:()=>o});var a=r(7294),n=r(6010);const l={tabItem:"tabItem_Ymn6"};function o(e){let{children:t,hidden:r,className:o}=e;return a.createElement("div",{role:"tabpanel",className:(0,n.Z)(l.tabItem,o),hidden:r},t)}},4866:(e,t,r)=>{r.d(t,{Z:()=>w});var a=r(7462),n=r(7294),l=r(6010),o=r(2466),s=r(6550),i=r(1980),u=r(7392),c=r(12);function p(e){return function(e){return n.Children.map(e,(e=>{if(!e||(0,n.isValidElement)(e)&&function(e){const{props:t}=e;return!!t&&"object"==typeof t&&"value"in t}(e))return e;throw new Error(`Docusaurus error: Bad <Tabs> child <${"string"==typeof e.type?e.type:e.type.name}>: all children of the <Tabs> component should be <TabItem>, and every <TabItem> should have a unique "value" prop.`)}))?.filter(Boolean)??[]}(e).map((e=>{let{props:{value:t,label:r,attributes:a,default:n}}=e;return{value:t,label:r,attributes:a,default:n}}))}function m(e){const{values:t,children:r}=e;return(0,n.useMemo)((()=>{const e=t??p(r);return function(e){const t=(0,u.l)(e,((e,t)=>e.value===t.value));if(t.length>0)throw new Error(`Docusaurus error: Duplicate values "${t.map((e=>e.value)).join(", ")}" found in <Tabs>. Every value needs to be unique.`)}(e),e}),[t,r])}function d(e){let{value:t,tabValues:r}=e;return r.some((e=>e.value===t))}function f(e){let{queryString:t=!1,groupId:r}=e;const a=(0,s.k6)(),l=function(e){let{queryString:t=!1,groupId:r}=e;if("string"==typeof t)return t;if(!1===t)return null;if(!0===t&&!r)throw new Error('Docusaurus error: The <Tabs> component groupId prop is required if queryString=true, because this value is used as the search param name. You can also provide an explicit value such as queryString="my-search-param".');return r??null}({queryString:t,groupId:r});return[(0,i._X)(l),(0,n.useCallback)((e=>{if(!l)return;const t=new URLSearchParams(a.location.search);t.set(l,e),a.replace({...a.location,search:t.toString()})}),[l,a])]}function b(e){const{defaultValue:t,queryString:r=!1,groupId:a}=e,l=m(e),[o,s]=(0,n.useState)((()=>function(e){let{defaultValue:t,tabValues:r}=e;if(0===r.length)throw new Error("Docusaurus error: the <Tabs> component requires at least one <TabItem> children component");if(t){if(!d({value:t,tabValues:r}))throw new Error(`Docusaurus error: The <Tabs> has a defaultValue "${t}" but none of its children has the corresponding value. Available values are: ${r.map((e=>e.value)).join(", ")}. If you intend to show no default tab, use defaultValue={null} instead.`);return t}const a=r.find((e=>e.default))??r[0];if(!a)throw new Error("Unexpected error: 0 tabValues");return a.value}({defaultValue:t,tabValues:l}))),[i,u]=f({queryString:r,groupId:a}),[p,b]=function(e){let{groupId:t}=e;const r=function(e){return e?`docusaurus.tab.${e}`:null}(t),[a,l]=(0,c.Nk)(r);return[a,(0,n.useCallback)((e=>{r&&l.set(e)}),[r,l])]}({groupId:a}),g=(()=>{const e=i??p;return d({value:e,tabValues:l})?e:null})();(0,n.useLayoutEffect)((()=>{g&&s(g)}),[g]);return{selectedValue:o,selectValue:(0,n.useCallback)((e=>{if(!d({value:e,tabValues:l}))throw new Error(`Can't select invalid tab value=${e}`);s(e),u(e),b(e)}),[u,b,l]),tabValues:l}}var g=r(2389);const h={tabList:"tabList__CuJ",tabItem:"tabItem_LNqP"};function k(e){let{className:t,block:r,selectedValue:s,selectValue:i,tabValues:u}=e;const c=[],{blockElementScrollPositionUntilNextRender:p}=(0,o.o5)(),m=e=>{const t=e.currentTarget,r=c.indexOf(t),a=u[r].value;a!==s&&(p(t),i(a))},d=e=>{let t=null;switch(e.key){case"Enter":m(e);break;case"ArrowRight":{const r=c.indexOf(e.currentTarget)+1;t=c[r]??c[0];break}case"ArrowLeft":{const r=c.indexOf(e.currentTarget)-1;t=c[r]??c[c.length-1];break}}t?.focus()};return n.createElement("ul",{role:"tablist","aria-orientation":"horizontal",className:(0,l.Z)("tabs",{"tabs--block":r},t)},u.map((e=>{let{value:t,label:r,attributes:o}=e;return n.createElement("li",(0,a.Z)({role:"tab",tabIndex:s===t?0:-1,"aria-selected":s===t,key:t,ref:e=>c.push(e),onKeyDown:d,onClick:m},o,{className:(0,l.Z)("tabs__item",h.tabItem,o?.className,{"tabs__item--active":s===t})}),r??t)})))}function y(e){let{lazy:t,children:r,selectedValue:a}=e;const l=(Array.isArray(r)?r:[r]).filter(Boolean);if(t){const e=l.find((e=>e.props.value===a));return e?(0,n.cloneElement)(e,{className:"margin-top--md"}):null}return n.createElement("div",{className:"margin-top--md"},l.map(((e,t)=>(0,n.cloneElement)(e,{key:t,hidden:e.props.value!==a}))))}function v(e){const t=b(e);return n.createElement("div",{className:(0,l.Z)("tabs-container",h.tabList)},n.createElement(k,(0,a.Z)({},e,t)),n.createElement(y,(0,a.Z)({},e,t)))}function w(e){const t=(0,g.Z)();return n.createElement(v,(0,a.Z)({key:String(t)},e))}},972:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>c,contentTitle:()=>i,default:()=>f,frontMatter:()=>s,metadata:()=>u,toc:()=>p});var a=r(7462),n=(r(7294),r(3905)),l=r(4866),o=r(5162);const s={sidebar_position:300},i="Transform",u={unversionedId:"userguide/transform",id:"version-0.8.0/userguide/transform",title:"Transform",description:"Once your data is ingested, you may start to expose insights by joining them and / or create meaningful aggregates.",source:"@site/versioned_docs/version-0.8.0/userguide/300.transform.md",sourceDirName:"userguide",slug:"/userguide/transform",permalink:"/starlake/docs/userguide/transform",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/versioned_docs/version-0.8.0/userguide/300.transform.md",tags:[],version:"0.8.0",sidebarPosition:300,frontMatter:{sidebar_position:300},sidebar:"starlakeSidebar",previous:{title:"Load",permalink:"/starlake/docs/userguide/load"},next:{title:"Configuration",permalink:"/starlake/docs/reference/configuration"}},c={},p=[{value:"Parquet Transform",id:"parquet-transform",level:2},{value:"BigQuery Transform",id:"bigquery-transform",level:2},{value:"Transform lineage",id:"transform-lineage",level:2}],m={toc:p},d="wrapper";function f(e){let{components:t,...s}=e;return(0,n.kt)(d,(0,a.Z)({},m,s,{components:t,mdxType:"MDXLayout"}),(0,n.kt)("h1",{id:"transform"},"Transform"),(0,n.kt)("p",null,"Once your data is ingested, you may start to expose insights by joining them and / or create meaningful aggregates."),(0,n.kt)("p",null,"In the example below, we join the ",(0,n.kt)("inlineCode",{parentName:"p"},"sellers")," and ",(0,n.kt)("inlineCode",{parentName:"p"},"orders")," tables to compute the total amount sold by each seller."),(0,n.kt)("p",null,"We want to do it on parquet files and on BigQuery. We need to create 2 env files, one per environment."),(0,n.kt)("h2",{id:"parquet-transform"},"Parquet Transform"),(0,n.kt)("p",null,"Create the ",(0,n.kt)("inlineCode",{parentName:"p"},"env.FS.comet.yml")," file in the ",(0,n.kt)("inlineCode",{parentName:"p"},"metadata")," folder as follows:"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-yaml"},'env:\n  myConnectionRef: "LocalFileSystem"\n')),(0,n.kt)("p",null,"Create in teh folder ",(0,n.kt)("inlineCode",{parentName:"p"},"metadata/transform/kpi/")," the SQL file that describe the job and name it ",(0,n.kt)("inlineCode",{parentName:"p"},"sellers_kpi.comet.yml"),":"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-SQL"},"select seller_email, sum(amount) as sum from sellers, orders where sellers.id = orders.seller_id group by sellers.seller_email\n")),(0,n.kt)("p",null,"Before executing the job, we set the ",(0,n.kt)("inlineCode",{parentName:"p"},"SL_ENV")," variable to ",(0,n.kt)("inlineCode",{parentName:"p"},"FS")," to make sure variables are instantiated correctly:"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-shell"},"export SL_ENV=FS\n$ starlake.sh transform --name kpi.sellers_kpi\n")),(0,n.kt)("h2",{id:"bigquery-transform"},"BigQuery Transform"),(0,n.kt)("p",null,"To execute the same request on BigQuery,  create the ",(0,n.kt)("inlineCode",{parentName:"p"},"env.BQ.comet.yml")," file in the ",(0,n.kt)("inlineCode",{parentName:"p"},"metadata")," folder as follows:"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-yaml"},'env:\n  myConnectionRef: "bigquery"\n')),(0,n.kt)("p",null,"Before executing the job, we set the ",(0,n.kt)("inlineCode",{parentName:"p"},"SL_ENV")," variable to ",(0,n.kt)("inlineCode",{parentName:"p"},"BQ")," to make sure variables are instantiated correctly:"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-shell"},"export SL_ENV=BQ\n$ starlake.sh transform --name kpi.sellers_kpi\n")),(0,n.kt)("h2",{id:"transform-lineage"},"Transform lineage"),(0,n.kt)("p",null,"To display the job linage, run the following command:"),(0,n.kt)(l.Z,{groupId:"platforms",mdxType:"Tabs"},(0,n.kt)(o.Z,{value:"linux_macos",label:"Linux/MacOS",mdxType:"TabItem"},(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-sh"},"$ cd $HOME/myproject\n$ $HOME/starlake/starlake.sh jobs2gv\n"))),(0,n.kt)(o.Z,{value:"windows",label:"Windows",mdxType:"TabItem"},(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-powershell"},"c:\\users\\me\\starlake> %userprofile%\\starlake\\starlake.cmd jobs2gv\n"))),(0,n.kt)(o.Z,{value:"docker",label:"Docker",mdxType:"TabItem"},(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-sh"},"$ docker run                                                        \\\n-e SL_ROOT=/app/myproject                                    \\\n-v $HOME/myproject:/app/myproject -it starlake jobs2gv\n")))),(0,n.kt)("p",null,"This will display the dot file. Copy paste de result to the following URL : ",(0,n.kt)("a",{parentName:"p",href:"https://dreampuf.github.io/GraphvizOnline/"},"https://dreampuf.github.io/GraphvizOnline/")),(0,n.kt)("p",null,(0,n.kt)("img",{src:r(4224).Z,width:"2968",height:"1198"})),(0,n.kt)("admonition",{type:"note"},(0,n.kt)("p",{parentName:"admonition"},"You may also convert this file to a png/svg/... file using the ",(0,n.kt)("inlineCode",{parentName:"p"},"dot")," command available at ",(0,n.kt)("a",{parentName:"p",href:"https://graphviz.org/download/"},"https://graphviz.org/download/"))))}f.isMDXComponent=!0},4224:(e,t,r)=>{r.d(t,{Z:()=>a});const a=r.p+"assets/images/graphviz-transform-b6eb963d92288a96036365a1074e2476.png"}}]);