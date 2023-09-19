"use strict";(self.webpackChunkstarlake_docs=self.webpackChunkstarlake_docs||[]).push([[5872],{3905:(e,t,a)=>{a.d(t,{Zo:()=>d,kt:()=>k});var n=a(7294);function l(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function r(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function i(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?r(Object(a),!0).forEach((function(t){l(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):r(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function o(e,t){if(null==e)return{};var a,n,l=function(e,t){if(null==e)return{};var a,n,l={},r=Object.keys(e);for(n=0;n<r.length;n++)a=r[n],t.indexOf(a)>=0||(l[a]=e[a]);return l}(e,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(n=0;n<r.length;n++)a=r[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(l[a]=e[a])}return l}var s=n.createContext({}),m=function(e){var t=n.useContext(s),a=t;return e&&(a="function"==typeof e?e(t):i(i({},t),e)),a},d=function(e){var t=m(e.components);return n.createElement(s.Provider,{value:t},e.children)},p="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},c=n.forwardRef((function(e,t){var a=e.components,l=e.mdxType,r=e.originalType,s=e.parentName,d=o(e,["components","mdxType","originalType","parentName"]),p=m(a),c=l,k=p["".concat(s,".").concat(c)]||p[c]||u[c]||r;return a?n.createElement(k,i(i({ref:t},d),{},{components:a})):n.createElement(k,i({ref:t},d))}));function k(e,t){var a=arguments,l=t&&t.mdxType;if("string"==typeof e||l){var r=a.length,i=new Array(r);i[0]=c;var o={};for(var s in t)hasOwnProperty.call(t,s)&&(o[s]=t[s]);o.originalType=e,o[p]="string"==typeof e?e:l,i[1]=o;for(var m=2;m<r;m++)i[m]=a[m];return n.createElement.apply(null,i)}return n.createElement.apply(null,a)}c.displayName="MDXCreateElement"},5162:(e,t,a)=>{a.d(t,{Z:()=>i});var n=a(7294),l=a(6010);const r={tabItem:"tabItem_Ymn6"};function i(e){let{children:t,hidden:a,className:i}=e;return n.createElement("div",{role:"tabpanel",className:(0,l.Z)(r.tabItem,i),hidden:a},t)}},4866:(e,t,a)=>{a.d(t,{Z:()=>v});var n=a(7462),l=a(7294),r=a(6010),i=a(2466),o=a(6550),s=a(1980),m=a(7392),d=a(12);function p(e){return function(e){return l.Children.map(e,(e=>{if(!e||(0,l.isValidElement)(e)&&function(e){const{props:t}=e;return!!t&&"object"==typeof t&&"value"in t}(e))return e;throw new Error(`Docusaurus error: Bad <Tabs> child <${"string"==typeof e.type?e.type:e.type.name}>: all children of the <Tabs> component should be <TabItem>, and every <TabItem> should have a unique "value" prop.`)}))?.filter(Boolean)??[]}(e).map((e=>{let{props:{value:t,label:a,attributes:n,default:l}}=e;return{value:t,label:a,attributes:n,default:l}}))}function u(e){const{values:t,children:a}=e;return(0,l.useMemo)((()=>{const e=t??p(a);return function(e){const t=(0,m.l)(e,((e,t)=>e.value===t.value));if(t.length>0)throw new Error(`Docusaurus error: Duplicate values "${t.map((e=>e.value)).join(", ")}" found in <Tabs>. Every value needs to be unique.`)}(e),e}),[t,a])}function c(e){let{value:t,tabValues:a}=e;return a.some((e=>e.value===t))}function k(e){let{queryString:t=!1,groupId:a}=e;const n=(0,o.k6)(),r=function(e){let{queryString:t=!1,groupId:a}=e;if("string"==typeof t)return t;if(!1===t)return null;if(!0===t&&!a)throw new Error('Docusaurus error: The <Tabs> component groupId prop is required if queryString=true, because this value is used as the search param name. You can also provide an explicit value such as queryString="my-search-param".');return a??null}({queryString:t,groupId:a});return[(0,s._X)(r),(0,l.useCallback)((e=>{if(!r)return;const t=new URLSearchParams(n.location.search);t.set(r,e),n.replace({...n.location,search:t.toString()})}),[r,n])]}function h(e){const{defaultValue:t,queryString:a=!1,groupId:n}=e,r=u(e),[i,o]=(0,l.useState)((()=>function(e){let{defaultValue:t,tabValues:a}=e;if(0===a.length)throw new Error("Docusaurus error: the <Tabs> component requires at least one <TabItem> children component");if(t){if(!c({value:t,tabValues:a}))throw new Error(`Docusaurus error: The <Tabs> has a defaultValue "${t}" but none of its children has the corresponding value. Available values are: ${a.map((e=>e.value)).join(", ")}. If you intend to show no default tab, use defaultValue={null} instead.`);return t}const n=a.find((e=>e.default))??a[0];if(!n)throw new Error("Unexpected error: 0 tabValues");return n.value}({defaultValue:t,tabValues:r}))),[s,m]=k({queryString:a,groupId:n}),[p,h]=function(e){let{groupId:t}=e;const a=function(e){return e?`docusaurus.tab.${e}`:null}(t),[n,r]=(0,d.Nk)(a);return[n,(0,l.useCallback)((e=>{a&&r.set(e)}),[a,r])]}({groupId:n}),f=(()=>{const e=s??p;return c({value:e,tabValues:r})?e:null})();(0,l.useLayoutEffect)((()=>{f&&o(f)}),[f]);return{selectedValue:i,selectValue:(0,l.useCallback)((e=>{if(!c({value:e,tabValues:r}))throw new Error(`Can't select invalid tab value=${e}`);o(e),m(e),h(e)}),[m,h,r]),tabValues:r}}var f=a(2389);const g={tabList:"tabList__CuJ",tabItem:"tabItem_LNqP"};function y(e){let{className:t,block:a,selectedValue:o,selectValue:s,tabValues:m}=e;const d=[],{blockElementScrollPositionUntilNextRender:p}=(0,i.o5)(),u=e=>{const t=e.currentTarget,a=d.indexOf(t),n=m[a].value;n!==o&&(p(t),s(n))},c=e=>{let t=null;switch(e.key){case"Enter":u(e);break;case"ArrowRight":{const a=d.indexOf(e.currentTarget)+1;t=d[a]??d[0];break}case"ArrowLeft":{const a=d.indexOf(e.currentTarget)-1;t=d[a]??d[d.length-1];break}}t?.focus()};return l.createElement("ul",{role:"tablist","aria-orientation":"horizontal",className:(0,r.Z)("tabs",{"tabs--block":a},t)},m.map((e=>{let{value:t,label:a,attributes:i}=e;return l.createElement("li",(0,n.Z)({role:"tab",tabIndex:o===t?0:-1,"aria-selected":o===t,key:t,ref:e=>d.push(e),onKeyDown:c,onClick:u},i,{className:(0,r.Z)("tabs__item",g.tabItem,i?.className,{"tabs__item--active":o===t})}),a??t)})))}function N(e){let{lazy:t,children:a,selectedValue:n}=e;const r=(Array.isArray(a)?a:[a]).filter(Boolean);if(t){const e=r.find((e=>e.props.value===n));return e?(0,l.cloneElement)(e,{className:"margin-top--md"}):null}return l.createElement("div",{className:"margin-top--md"},r.map(((e,t)=>(0,l.cloneElement)(e,{key:t,hidden:e.props.value!==n}))))}function b(e){const t=h(e);return l.createElement("div",{className:(0,r.Z)("tabs-container",g.tabList)},l.createElement(y,(0,n.Z)({},e,t)),l.createElement(N,(0,n.Z)({},e,t)))}function v(e){const t=(0,f.Z)();return l.createElement(b,(0,n.Z)({key:String(t)},e))}},346:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>d,contentTitle:()=>s,default:()=>k,frontMatter:()=>o,metadata:()=>m,toc:()=>p});var n=a(7462),l=(a(7294),a(3905)),r=a(4866),i=a(5162);const o={sidebar_position:100,title:"Load"},s=void 0,m={unversionedId:"quickstart/load",id:"version-0.8.0/quickstart/load",title:"Load",description:"In this section you will learn how to load and transform data using the samples files created when bootstrapping a new project.",source:"@site/versioned_docs/version-0.8.0/quickstart/100.load.mdx",sourceDirName:"quickstart",slug:"/quickstart/load",permalink:"/starlake/docs/0.8.0/quickstart/load",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/versioned_docs/version-0.8.0/quickstart/100.load.mdx",tags:[],version:"0.8.0",sidebarPosition:100,frontMatter:{sidebar_position:100,title:"Load"},sidebar:"starlakeSidebar",previous:{title:"Bootstrap project",permalink:"/starlake/docs/0.8.0/quickstart/bootstrap"},next:{title:"Transform",permalink:"/starlake/docs/0.8.0/quickstart/transform"}},d={},p=[{value:"Sample scenario",id:"sample-scenario",level:2},{value:"customers dataset",id:"customers-dataset",level:4},{value:"Infer the schema",id:"infer-the-schema",level:2},{value:"Domain configuration file: _config.sl.yml",id:"domain-configuration-file-_configslyml",level:3},{value:"Table configuration file: customers.sl.yml",id:"table-configuration-file-customersslyml",level:3},{value:"Load files",id:"load-files",level:2},{value:"Import step",id:"import-step",level:3},{value:"Load step",id:"load-step",level:3},{value:"Check the result",id:"check-the-result",level:3},{value:"Configuration",id:"configuration",level:2},{value:"How the result went tot he filesystem as parquet files",id:"how-the-result-went-tot-he-filesystem-as-parquet-files",level:3},{value:"Loading the data into BigQuery",id:"loading-the-data-into-bigquery",level:3}],u={toc:p},c="wrapper";function k(e){let{components:t,...a}=e;return(0,l.kt)(c,(0,n.Z)({},u,a,{components:t,mdxType:"MDXLayout"}),(0,l.kt)("p",null,"In this section you will learn how to load and transform data using the samples files created when ",(0,l.kt)("a",{parentName:"p",href:"bootstrap"},"bootstrapping a new project"),".\nYou will learn how to:"),(0,l.kt)("ul",null,(0,l.kt)("li",{parentName:"ul"},"import: recognize new arriving files that need to be loaded into your warehouse"),(0,l.kt)("li",{parentName:"ul"},"load: validate file records and load data into you warehouse"),(0,l.kt)("li",{parentName:"ul"},"transform: apply transformation on your previously loaded data")),(0,l.kt)("h2",{id:"sample-scenario"},"Sample scenario"),(0,l.kt)("p",null,"Say we have to load the ",(0,l.kt)("inlineCode",{parentName:"p"},"customers"),' file into the warehouse.\nThe customers are provided by the "sales" department as delimited separated values files\nand are required to be loaded incrementally.'),(0,l.kt)("h4",{id:"customers-dataset"},"customers dataset"),(0,l.kt)("p",null,(0,l.kt)("inlineCode",{parentName:"p"},'File customers-2018-01-01.psv from "sales" department')),(0,l.kt)("table",null,(0,l.kt)("thead",{parentName:"table"},(0,l.kt)("tr",{parentName:"thead"},(0,l.kt)("th",{parentName:"tr",align:null},"id"),(0,l.kt)("th",{parentName:"tr",align:null},"signup"),(0,l.kt)("th",{parentName:"tr",align:null},"contact"),(0,l.kt)("th",{parentName:"tr",align:null},"birthdate"),(0,l.kt)("th",{parentName:"tr",align:null},"firstname"),(0,l.kt)("th",{parentName:"tr",align:null},"lastname"),(0,l.kt)("th",{parentName:"tr",align:null},"country"))),(0,l.kt)("tbody",{parentName:"table"},(0,l.kt)("tr",{parentName:"tbody"},(0,l.kt)("td",{parentName:"tr",align:null},"A009701"),(0,l.kt)("td",{parentName:"tr",align:null},"2010-01-31 23:04:15"),(0,l.kt)("td",{parentName:"tr",align:null},(0,l.kt)("a",{parentName:"td",href:"mailto:k@m.com"},"k@m.com")),(0,l.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,l.kt)("td",{parentName:"tr",align:null},"Kylian"),(0,l.kt)("td",{parentName:"tr",align:null},"Mbapp\xe9"),(0,l.kt)("td",{parentName:"tr",align:null},"France")),(0,l.kt)("tr",{parentName:"tbody"},(0,l.kt)("td",{parentName:"tr",align:null},"B000001"),(0,l.kt)("td",{parentName:"tr",align:null},"2016-12-01 09:56:02"),(0,l.kt)("td",{parentName:"tr",align:null},(0,l.kt)("a",{parentName:"td",href:"mailto:n@b.com"},"n@b.com")),(0,l.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,l.kt)("td",{parentName:"tr",align:null},"Napoleon"),(0,l.kt)("td",{parentName:"tr",align:null},"Bonaparte"),(0,l.kt)("td",{parentName:"tr",align:null},"France")),(0,l.kt)("tr",{parentName:"tbody"},(0,l.kt)("td",{parentName:"tr",align:null},"B000001"),(0,l.kt)("td",{parentName:"tr",align:null},"2016-12-02 09:56:02"),(0,l.kt)("td",{parentName:"tr",align:null},(0,l.kt)("a",{parentName:"td",href:"mailto:m@c.com"},"m@c.com")),(0,l.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,l.kt)("td",{parentName:"tr",align:null},"Marie"),(0,l.kt)("td",{parentName:"tr",align:null},"Curie"),(0,l.kt)("td",{parentName:"tr",align:null},"France")),(0,l.kt)("tr",{parentName:"tbody"},(0,l.kt)("td",{parentName:"tr",align:null},"B000002"),(0,l.kt)("td",{parentName:"tr",align:null},"2016-12-02 09:56:02"),(0,l.kt)("td",{parentName:"tr",align:null},(0,l.kt)("a",{parentName:"td",href:"mailto:z@z.com"},"z@z.com")),(0,l.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,l.kt)("td",{parentName:"tr",align:null},"Zinedine"),(0,l.kt)("td",{parentName:"tr",align:null},"Zidane"),(0,l.kt)("td",{parentName:"tr",align:null},"France")),(0,l.kt)("tr",{parentName:"tbody"},(0,l.kt)("td",{parentName:"tr",align:null},"B000003"),(0,l.kt)("td",{parentName:"tr",align:null},"2016-12-03 09:56:02"),(0,l.kt)("td",{parentName:"tr",align:null},(0,l.kt)("a",{parentName:"td",href:"mailto:e@g.com"},"e@g.com")),(0,l.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,l.kt)("td",{parentName:"tr",align:null},"Eva"),(0,l.kt)("td",{parentName:"tr",align:null},"Green"),(0,l.kt)("td",{parentName:"tr",align:null},"France")),(0,l.kt)("tr",{parentName:"tbody"},(0,l.kt)("td",{parentName:"tr",align:null},"B000012"),(0,l.kt)("td",{parentName:"tr",align:null},"2016-12-03 09:56:02"),(0,l.kt)("td",{parentName:"tr",align:null},(0,l.kt)("a",{parentName:"td",href:"mailto:k@b.com"},"k@b.com")),(0,l.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,l.kt)("td",{parentName:"tr",align:null},"Karim"),(0,l.kt)("td",{parentName:"tr",align:null},"Benzema"),(0,l.kt)("td",{parentName:"tr",align:null},"France")),(0,l.kt)("tr",{parentName:"tbody"},(0,l.kt)("td",{parentName:"tr",align:null},"B000004"),(0,l.kt)("td",{parentName:"tr",align:null},"2016-12-04 09:56:02"),(0,l.kt)("td",{parentName:"tr",align:null},(0,l.kt)("a",{parentName:"td",href:"mailto:m@c.com"},"m@c.com")),(0,l.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,l.kt)("td",{parentName:"tr",align:null},"Marion"),(0,l.kt)("td",{parentName:"tr",align:null},"Cotillard"),(0,l.kt)("td",{parentName:"tr",align:null},"France")),(0,l.kt)("tr",{parentName:"tbody"},(0,l.kt)("td",{parentName:"tr",align:null},"B000005"),(0,l.kt)("td",{parentName:"tr",align:null},"2016-12-05 09:56:02"),(0,l.kt)("td",{parentName:"tr",align:null},(0,l.kt)("a",{parentName:"td",href:"mailto:a@g.com"},"a@g.com")),(0,l.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,l.kt)("td",{parentName:"tr",align:null},"Ariana"),(0,l.kt)("td",{parentName:"tr",align:null},"Grande"),(0,l.kt)("td",{parentName:"tr",align:null},"USA")),(0,l.kt)("tr",{parentName:"tbody"},(0,l.kt)("td",{parentName:"tr",align:null},"B000006"),(0,l.kt)("td",{parentName:"tr",align:null},"2016-12-06 09:56:02"),(0,l.kt)("td",{parentName:"tr",align:null},(0,l.kt)("a",{parentName:"td",href:"mailto:m@j.com"},"m@j.com")),(0,l.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,l.kt)("td",{parentName:"tr",align:null},"Michael"),(0,l.kt)("td",{parentName:"tr",align:null},"Jordan"),(0,l.kt)("td",{parentName:"tr",align:null},"USA")),(0,l.kt)("tr",{parentName:"tbody"},(0,l.kt)("td",{parentName:"tr",align:null},"B000007"),(0,l.kt)("td",{parentName:"tr",align:null},"2016-12-07 09:56:02"),(0,l.kt)("td",{parentName:"tr",align:null},(0,l.kt)("a",{parentName:"td",href:"mailto:m@a.com"},"m@a.com")),(0,l.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,l.kt)("td",{parentName:"tr",align:null},"Muhammad"),(0,l.kt)("td",{parentName:"tr",align:null},"Ali"),(0,l.kt)("td",{parentName:"tr",align:null},"USA")),(0,l.kt)("tr",{parentName:"tbody"},(0,l.kt)("td",{parentName:"tr",align:null},"B000008"),(0,l.kt)("td",{parentName:"tr",align:null},"2016-12-08 09:56:02"),(0,l.kt)("td",{parentName:"tr",align:null},(0,l.kt)("a",{parentName:"td",href:"mailto:t@s.com"},"t@s.com")),(0,l.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,l.kt)("td",{parentName:"tr",align:null},"Taylor"),(0,l.kt)("td",{parentName:"tr",align:null},"Swift"),(0,l.kt)("td",{parentName:"tr",align:null},"USA")),(0,l.kt)("tr",{parentName:"tbody"},(0,l.kt)("td",{parentName:"tr",align:null},"B000009"),(0,l.kt)("td",{parentName:"tr",align:null},"2016-12-09 09:56:02"),(0,l.kt)("td",{parentName:"tr",align:null},(0,l.kt)("a",{parentName:"td",href:"mailto:e@p.com"},"e@p.com")),(0,l.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,l.kt)("td",{parentName:"tr",align:null},"Elvis"),(0,l.kt)("td",{parentName:"tr",align:null},"Presley"),(0,l.kt)("td",{parentName:"tr",align:null},"USA")),(0,l.kt)("tr",{parentName:"tbody"},(0,l.kt)("td",{parentName:"tr",align:null},"B000010"),(0,l.kt)("td",{parentName:"tr",align:null},"2016-12-10 09:56:02"),(0,l.kt)("td",{parentName:"tr",align:null},(0,l.kt)("a",{parentName:"td",href:"mailto:s@j.com"},"s@j.com")),(0,l.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,l.kt)("td",{parentName:"tr",align:null},"Steve"),(0,l.kt)("td",{parentName:"tr",align:null},"Jobs"),(0,l.kt)("td",{parentName:"tr",align:null},"USA")),(0,l.kt)("tr",{parentName:"tbody"},(0,l.kt)("td",{parentName:"tr",align:null},"B000011"),(0,l.kt)("td",{parentName:"tr",align:null},"2016-12-11 09:56:02"),(0,l.kt)("td",{parentName:"tr",align:null},(0,l.kt)("a",{parentName:"td",href:"mailto:a@l.com"},"a@l.com")),(0,l.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,l.kt)("td",{parentName:"tr",align:null},"Abraham"),(0,l.kt)("td",{parentName:"tr",align:null},"Lincoln"),(0,l.kt)("td",{parentName:"tr",align:null},"USA")))),(0,l.kt)("h2",{id:"infer-the-schema"},"Infer the schema"),(0,l.kt)("p",null,"In Starlake terms, after loading, we will end up with:"),(0,l.kt)("ul",null,(0,l.kt)("li",{parentName:"ul"},"one domain: ",(0,l.kt)("inlineCode",{parentName:"li"},"sales")," . A domain is equivalent to a database schema or a BigQuery dataset."),(0,l.kt)("li",{parentName:"ul"},"one table: the ",(0,l.kt)("inlineCode",{parentName:"li"},"customers")," table in the ",(0,l.kt)("inlineCode",{parentName:"li"},"sales")," domain")),(0,l.kt)("p",null,"We first need to write a YAML configuration file that describe the structure of the file to load into the warehouse.\nInstead of writing this file by hand, we may infer this YAML configuration file using the ",(0,l.kt)("inlineCode",{parentName:"p"},"infer-schema")," command."),(0,l.kt)(r.Z,{groupId:"platforms",mdxType:"Tabs"},(0,l.kt)(i.Z,{value:"linux_macos",label:"Linux/MacOS",mdxType:"TabItem"},(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sh"},"$ cd $HOME/myproject\n$ $HOME/starlake/starlake.sh infer-schema               \\\n    --domain sales                                      \\\n    --table customers                                   \\\n    --input incoming/customers-2018-01-01.psv           \\\n    --with-header\n"))),(0,l.kt)(i.Z,{value:"windows",label:"Windows",mdxType:"TabItem"},(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-powershell"},"c:\\users\\me\\starlake> %userprofile%\\starlake\\starlake.cmd infer-schema  ^\n    --domain sales                                                      ^\n    --table customers                                                   ^\n    --input incoming/customers-2018-01-01.psv                           ^\n    --with-header\n"))),(0,l.kt)(i.Z,{value:"docker",label:"Docker",mdxType:"TabItem"},(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-shell"},"$ docker run                                                        \\\n    -e SL_ROOT=/app/myproject                                       \\\n    -v $HOME/myproject:/app/myproject -it starlake infer-schema     \\\n    --domain sales                                                  \\\n    --table customers                                               \\\n    --input $HOME/myproject/incoming/customers-2018-01-01.psv       \\\n    --with-header```\n")))),(0,l.kt)("p",null,"This inferred YAML file may be found in the ",(0,l.kt)("inlineCode",{parentName:"p"},"customers.sl.yml")," file under the ",(0,l.kt)("inlineCode",{parentName:"p"},"metadata/load/sales")," folder.\nNotice how the ",(0,l.kt)("inlineCode",{parentName:"p"},"customers.sl.yml")," file is named after the ",(0,l.kt)("em",{parentName:"p"},"table")," name and stored in the folder named after the ",(0,l.kt)("em",{parentName:"p"},"domain")," name.\nThe domain configuration file _",(0,l.kt)("em",{parentName:"p"},"config.sl.yml")," is also stored in the ",(0,l.kt)("em",{parentName:"p"},"domain")," folder."),(0,l.kt)("p",null,"The contents of the files look like this:"),(0,l.kt)("h3",{id:"domain-configuration-file-_configslyml"},"Domain configuration file: _config.sl.yml"),(0,l.kt)("p",null,"This file describes the properties shared by all tables in this domain.\nHere we assume that all tables in the ",(0,l.kt)("inlineCode",{parentName:"p"},"sales")," domain are loaded from the ",(0,l.kt)("em",{parentName:"p"},"incoming")," folder."),(0,l.kt)("p",null,(0,l.kt)("inlineCode",{parentName:"p"},"{{root_path}}")," is a variable path set in the env file (more on this later)."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="Domain configuration file: _config.sl.yml"',title:'"Domain',configuration:!0,"file:":!0,'_config.sl.yml"':!0},'---\nload:\n  name: "sales"\n  metadata:\n    directory: "{{root_path}}/incoming"\n')),(0,l.kt)("p",null,"You may change the path referenced by the ",(0,l.kt)("em",{parentName:"p"},"directory")," attribute to any other path."),(0,l.kt)("h3",{id:"table-configuration-file-customersslyml"},"Table configuration file: customers.sl.yml"),(0,l.kt)("p",null,"The YAML file describes the structure of the file to load into the warehouse.\nThe ",(0,l.kt)("inlineCode",{parentName:"p"},"pattern")," property is a regular expression that will be used to match the file name."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="Table schema: customers.sl.yml"',title:'"Table',"schema:":!0,'customers.sl.yml"':!0},'---\ntable:\n  name: "customers"   # destination table name\n  pattern: "customers.*.psv" # This property is a regular expression that will be used to match the file name.\n                                      # Please replace it by the adequate file pattern eq. customers-.*.psv if required\n  attributes:         # Description of the fields to recognize\n    - name: "id"        # attribute name and column name in the destination table if no rename attribute is defined\n      type: "string"    # expected type\n      array: false      # is it an array (false by default, ignored in DSV files) ?\n      required: false   # Is this field required in the source (false by default, change it accordingly) ?\n      privacy: "NONE"   # Should we encrypt this field before loading to the warehouse (No encryption by default )?\n      ignore: false     # Should this field be excluded (false by default) ?\n    - name: "signup"    # second attribute\n      type: "timestamp" # recognized type by analyzing input.\n    - name: "contact"\n      type: "string"\n      # ...\n    - name: "birthdate"\n      type: "date"      # recognized as semantic type date.\n      # ...\n    - name: "firstname"\n      type: "string"\n      # ...\n    - name: "lastname"\n      type: "string"\n      # ...\n    - name: "country"\n      type: "string"\n      # ...               # and so on ...\n  metadata:\n    mode: "FILE"\n    format: "DSV"\n    encoding: "UTF-8"\n    multiline: false\n    array: false\n    withHeader: true\n    separator: "|"\n    quote: "\\""\n    escape: "\\\\"\n\n')),(0,l.kt)("p",null,"We have seen in the ",(0,l.kt)("a",{parentName:"p",href:"bootstrap"},"bootstrap section")," that three environment files.\nThese environment files define the variables that will be used for runtime substitution in any YAML file. Variables in YAML files are enclosed with {{ and }}."),(0,l.kt)("admonition",{type:"note"},(0,l.kt)("p",{parentName:"admonition"},"You may also use the ",(0,l.kt)("em",{parentName:"p"},"${ }")," syntax to define variables.")),(0,l.kt)("p",null,"The environment file ",(0,l.kt)("inlineCode",{parentName:"p"},"metadata/env.sl.yml")," define the default variables.\nAny other environment file will be mixed with this default one and specific environment file have higher precedence.\nIn our example, we have the following environment files:"),(0,l.kt)("table",null,(0,l.kt)("thead",{parentName:"table"},(0,l.kt)("tr",{parentName:"thead"},(0,l.kt)("th",{parentName:"tr",align:null},"Environment"),(0,l.kt)("th",{parentName:"tr",align:null},"File used"))),(0,l.kt)("tbody",{parentName:"table"},(0,l.kt)("tr",{parentName:"tbody"},(0,l.kt)("td",{parentName:"tr",align:null},"default"),(0,l.kt)("td",{parentName:"tr",align:null},"env.sl.yml")),(0,l.kt)("tr",{parentName:"tbody"},(0,l.kt)("td",{parentName:"tr",align:null},"LOCAL"),(0,l.kt)("td",{parentName:"tr",align:null},"env.LOCAL.sl.yml, env.sl.yml")),(0,l.kt)("tr",{parentName:"tbody"},(0,l.kt)("td",{parentName:"tr",align:null},"BQ"),(0,l.kt)("td",{parentName:"tr",align:null},"env.BQ.sl.yml, env.sl.yml")))),(0,l.kt)("p",null,"The active environment is set through the ",(0,l.kt)("inlineCode",{parentName:"p"},"SL_ENV")," environment variable. For more details, have a look at the ",(0,l.kt)("a",{parentName:"p",href:"../reference/environment"},"environment reference")),(0,l.kt)("p",null,"In our example, the ",(0,l.kt)("inlineCode",{parentName:"p"},"root_path")," must be defined to the location where the incoming directory containing the datasets to load will be located.\nSet it to your project directory as follows in the default environment file:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre"},"env:\n  root_path: ${HOME}/myproject\n")),(0,l.kt)("p",null,"or simply leave it as ",(0,l.kt)("inlineCode",{parentName:"p"},"{{SL_ROOT}}"),"."),(0,l.kt)("p",null,(0,l.kt)("inlineCode",{parentName:"p"},"SL_ROOT")," is a special variable that is set to the directory from where the ",(0,l.kt)("inlineCode",{parentName:"p"},"starlake")," command is executed."),(0,l.kt)("h2",{id:"load-files"},"Load files"),(0,l.kt)("p",null,"Loading is a two step process: The optional ",(0,l.kt)("inlineCode",{parentName:"p"},"import")," step and the ",(0,l.kt)("inlineCode",{parentName:"p"},"load")," step."),(0,l.kt)("h3",{id:"import-step"},"Import step"),(0,l.kt)("p",null,"In this first step, Starlake will look at the ",(0,l.kt)("em",{parentName:"p"},"directory")," attribute value in the YAML file and look at the file that matches the expected pattern defined in the table definition.\nIn our example, the directory is ",(0,l.kt)("inlineCode",{parentName:"p"},"{{root_path}}/incoming")," and the expected file pattern has been changed to ",(0,l.kt)("inlineCode",{parentName:"p"},"customers.*.psv")),(0,l.kt)("p",null,"the ",(0,l.kt)("inlineCode",{parentName:"p"},"import")," command moves the files that satisfy one table pattern from the ",(0,l.kt)("em",{parentName:"p"},"incoming")," folder to the ",(0,l.kt)("inlineCode",{parentName:"p"},"datasets/pending")," folder.\nFiles that do not satisfy any pattern won't be loaded and are moved to the ",(0,l.kt)("inlineCode",{parentName:"p"},"datasets/unresolved")," directory."),(0,l.kt)(r.Z,{groupId:"platforms",mdxType:"Tabs"},(0,l.kt)(i.Z,{value:"linux_macos",label:"Linux/MacOS",mdxType:"TabItem"},(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sh"},"$ cd $HOME/myproject\n$ $HOME/starlake/starlake.sh import\n"))),(0,l.kt)(i.Z,{value:"windows",label:"Windows",mdxType:"TabItem"},(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-powershell"},"c:\\users\\me\\starlake> %userprofile%\\starlake\\starlake.cmd import\n"))),(0,l.kt)(i.Z,{value:"docker",label:"Docker",mdxType:"TabItem"},(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-shell"},"$ docker run                                                        \\\n    -e SL_ROOT=/app/myproject                                    \\\n    -v $HOME/myproject:/app/myproject -it starlake import\n")))),(0,l.kt)("p",null,"The ",(0,l.kt)("em",{parentName:"p"},"customers")," file has now been moved to the ",(0,l.kt)("inlineCode",{parentName:"p"},"myproject/datasets/pending/sales")," directory."),(0,l.kt)("admonition",{type:"note"},(0,l.kt)("p",{parentName:"admonition"},"This step is optional and does not need to be run if your files directly arrive in the ",(0,l.kt)("inlineCode",{parentName:"p"},"datasets/pending/sales")," folder.\nAlso note that all these source directories may be ",(0,l.kt)("a",{parentName:"p",href:"../reference/configuration"},"redefined"),".")),(0,l.kt)("h3",{id:"load-step"},"Load step"),(0,l.kt)("p",null,"In this second step, each line in the file present in the ",(0,l.kt)("inlineCode",{parentName:"p"},"datasets/pending")," folder is checked against the schema described in the YAML file\nand its result is stored in the warehouse."),(0,l.kt)(r.Z,{groupId:"platforms",mdxType:"Tabs"},(0,l.kt)(i.Z,{value:"linux_macos",label:"Linux/MacOS",mdxType:"TabItem"},(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sh"},"$ cd $HOME/myproject\n$ $HOME/starlake/starlake.sh load\n"))),(0,l.kt)(i.Z,{value:"windows",label:"Windows",mdxType:"TabItem"},(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-powershell"},"c:\\users\\me\\starlake> %userprofile%\\starlake\\starlake.cmd load\n"))),(0,l.kt)(i.Z,{value:"docker",label:"Docker",mdxType:"TabItem"},(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-shell"},"$ docker run                                                        \\\n    -e SL_ROOT=/app/myproject                                    \\\n    -v $HOME/myproject:/app/myproject -it starlake load\n")))),(0,l.kt)("p",null,"This will load the CSV file and store it as a parquet file into the following folders:"),(0,l.kt)("ul",null,(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"datasets/accepted")," for valid records"),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"datasets/rejected")," for invalid records"),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"datasets/unresolved")," for unrecognized files")),(0,l.kt)("admonition",{type:"caution"},(0,l.kt)("p",{parentName:"admonition"},"Starlake validate the data against the table's schema of the first pattern that match with the file name.\nHence, you must be careful regarding the pattern you set.\nMake sure that there is no overlap.")),(0,l.kt)("h3",{id:"check-the-result"},"Check the result"),(0,l.kt)("p",null,"You can check the result by running the following python script to read the parquet file from the current directory:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-python"},"import pandas as pd\nfilepath = 'datasets/accepted/sales/customers/'\ndf = pd.read_parquet(filepath)\ndf.head\n")),(0,l.kt)("h2",{id:"configuration"},"Configuration"),(0,l.kt)("h3",{id:"how-the-result-went-tot-he-filesystem-as-parquet-files"},"How the result went tot he filesystem as parquet files"),(0,l.kt)("p",null,"Starlake tried to find the definition of the active environment. It searches to it in the ",(0,l.kt)("inlineCode",{parentName:"p"},"SL_ENV")," variable.\nThis variable may be defined as:"),(0,l.kt)("ul",null,(0,l.kt)("li",{parentName:"ul"},"a value in the ",(0,l.kt)("inlineCode",{parentName:"li"},"metadata/env.sl.yml")," file"),(0,l.kt)("li",{parentName:"ul"},"a value passed as an environment variable"),(0,l.kt)("li",{parentName:"ul"},"a java property")),(0,l.kt)("p",null,"In our case this variable has been set as follows to the ",(0,l.kt)("em",{parentName:"p"},"LOCAL")," value."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'{3,3} title="Default env.sl.yml configuration file. Always loaded"',"{3,3}":!0,title:'"Default',"env.sl.yml":!0,configuration:!0,"file.":!0,Always:!0,'loaded"':!0},'env:\n  root_path: "{{SL_ROOT}}"\n  SL_ENV: LOCAL\n  myConnectionRef: "???"\n  bigQueryMaterializationDataset: "???" # need to be defined for spark loader on BigQuery only\n  loader: "???" # native or spark. Need to be defined in the specific env.{{SL_ENV}}.sl.yml file\n')),(0,l.kt)("p",null,"This means that the specific ",(0,l.kt)("em",{parentName:"p"},"env.LOCAL.sl.yml")," environment file will be loaded and mixed with the default one :"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="env.LOCAL.sl.yml is Loaded since SL_ENV is evaluated to LOCAL"',title:'"env.LOCAL.sl.yml',is:!0,Loaded:!0,since:!0,SL_ENV:!0,evaluated:!0,to:!0,'LOCAL"':!0},'env:\n  myConnectionRef: "localFilesystem" # use the local file system connection as defined in application.sl.yml\n  loader: "spark" # spark since we are running in local mode\n')),(0,l.kt)("p",null,"The ",(0,l.kt)("inlineCode",{parentName:"p"},"connectionRef")," refers to the datawarehouse connection defined in the ",(0,l.kt)("inlineCode",{parentName:"p"},"application.sl.yml")," file as follows:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'{3,4} title="application.sl.yml"',"{3,4}":!0,title:'"application.sl.yml"'},'application:\n  connections:\n    localFilesystem:\n      type: "fs"\n    bigquery:\n      type: "bigquery"\n      options:\n        #gcsBucket: "starlake-app" # Temporary GCS Bucket where intermediary files will be stored. Required in indirect mode only\n        writeMethod: "direct" # direct or indirect\n        authType: APPLICATION_DEFAULT\n        #authType: SERVICE_ACCOUNT_JSON_KEYFILE\n        #jsonKeyfile: "/Users/me/.gcloud/keys/starlake-me.json"\n\n  # default connection\n  connectionRef: "{{ myConnectionRef }}" # myConnectionRef is defined in the env.LOCAL.sl.yml & env.BQ.sl.yml files\n\n  loader: "{{ loader }}" # native or spark depending on the env.LOCAL.sl.yml & env.BQ.sl.yml files\n  spark:\n    datasource:\n      bigquery:\n        materializationDataset: {{ bigQueryMaterializationDataset }} # need to be defined for spark loader on BigQuery only\n\n')),(0,l.kt)("h3",{id:"loading-the-data-into-bigquery"},"Loading the data into BigQuery"),(0,l.kt)("p",null,"We just loaded our text file as a parquet file. This is a very common format for data scientists and analysts.\nThrough minimum extra configuration, we are able to run Starlake on top of any warehouse and have these datasets available as tables."),(0,l.kt)("p",null,"In our example, we will load the data into a BigQuery table.\ndefine the default env as being BQ (",(0,l.kt)("inlineCode",{parentName:"p"},"SL_ENV=BQ"),") in the ",(0,l.kt)("inlineCode",{parentName:"p"},"metadata/env.sl.yml")," file\nand set the values specific to BigQuery in the metadata/env.BQ.sl.yml file as follows:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="the env.BQ.sl.yml configuration file":',title:'"the',"env.BQ.sl.yml":!0,configuration:!0,'file":':!0},'env:\n  myConnectionRef: "bigquery" # Use the bigquery connection defined in the application.sl.yml file\n  loader: "native" # native or spark # native means we want to load the data using the BigQuery API\n')),(0,l.kt)("p",null,"To load the file using Spark simply set the loader to spark as follows:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="the env.BQ.sl.yml configuration file, used when SL_ENV is set to BQ":',title:'"the',"env.BQ.sl.yml":!0,configuration:!0,"file,":!0,used:!0,when:!0,SL_ENV:!0,is:!0,set:!0,to:!0,'BQ":':!0},'env:\n  myConnectionRef: "bigquery" # Use the bigquery connection defined in the application.sl.yml file\n  bigQueryMaterializationDataset: "BQ_TEST_DS" # Spark require a dataset to store temporary tables\n  loader: "spark" # Use Spark as the ingestion engine\n')),(0,l.kt)("p",null,"Using Spark instead of the BigQuery Load API may slow down the ingestion process but it has among others the following advantages:"),(0,l.kt)("ul",null,(0,l.kt)("li",{parentName:"ul"},"It allows to load data from any source supported by Spark including Fixed Width Files, XML files, JSON Arrays files ..."),(0,l.kt)("li",{parentName:"ul"},"It allows to load data into any destination supported by Spark including Snowflake, Amazon Redshift ..."),(0,l.kt)("li",{parentName:"ul"},"It allows to apply any transformation supported by Spark"),(0,l.kt)("li",{parentName:"ul"},"It allows to report any number of errors instead of 5 errors max with the BigQuery Load API (This is a BigQuery API Limitation)")),(0,l.kt)("p",null,"To load the data into BigQuery, simply put back the customer-*.psv file in the incoming folder\nand run the ",(0,l.kt)("em",{parentName:"p"},"import")," and ",(0,l.kt)("em",{parentName:"p"},"load")," commands again."))}k.isMDXComponent=!0}}]);