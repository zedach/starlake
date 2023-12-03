"use strict";(self.webpackChunkstarlake=self.webpackChunkstarlake||[]).push([[2138],{3905:(e,t,a)=>{a.d(t,{Zo:()=>d,kt:()=>k});var n=a(67294);function r(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function l(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function i(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?l(Object(a),!0).forEach((function(t){r(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):l(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function s(e,t){if(null==e)return{};var a,n,r=function(e,t){if(null==e)return{};var a,n,r={},l=Object.keys(e);for(n=0;n<l.length;n++)a=l[n],t.indexOf(a)>=0||(r[a]=e[a]);return r}(e,t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(n=0;n<l.length;n++)a=l[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(r[a]=e[a])}return r}var o=n.createContext({}),m=function(e){var t=n.useContext(o),a=t;return e&&(a="function"==typeof e?e(t):i(i({},t),e)),a},d=function(e){var t=m(e.components);return n.createElement(o.Provider,{value:t},e.children)},u="mdxType",p={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},c=n.forwardRef((function(e,t){var a=e.components,r=e.mdxType,l=e.originalType,o=e.parentName,d=s(e,["components","mdxType","originalType","parentName"]),u=m(a),c=r,k=u["".concat(o,".").concat(c)]||u[c]||p[c]||l;return a?n.createElement(k,i(i({ref:t},d),{},{components:a})):n.createElement(k,i({ref:t},d))}));function k(e,t){var a=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var l=a.length,i=new Array(l);i[0]=c;var s={};for(var o in t)hasOwnProperty.call(t,o)&&(s[o]=t[o]);s.originalType=e,s[u]="string"==typeof e?e:r,i[1]=s;for(var m=2;m<l;m++)i[m]=a[m];return n.createElement.apply(null,i)}return n.createElement.apply(null,a)}c.displayName="MDXCreateElement"},85162:(e,t,a)=>{a.d(t,{Z:()=>i});var n=a(67294),r=a(86010);const l={tabItem:"tabItem_Ymn6"};function i(e){let{children:t,hidden:a,className:i}=e;return n.createElement("div",{role:"tabpanel",className:(0,r.Z)(l.tabItem,i),hidden:a},t)}},74866:(e,t,a)=>{a.d(t,{Z:()=>v});var n=a(87462),r=a(67294),l=a(86010),i=a(12466),s=a(16550),o=a(91980),m=a(67392),d=a(50012);function u(e){return function(e){return r.Children.map(e,(e=>{if(!e||(0,r.isValidElement)(e)&&function(e){const{props:t}=e;return!!t&&"object"==typeof t&&"value"in t}(e))return e;throw new Error(`Docusaurus error: Bad <Tabs> child <${"string"==typeof e.type?e.type:e.type.name}>: all children of the <Tabs> component should be <TabItem>, and every <TabItem> should have a unique "value" prop.`)}))?.filter(Boolean)??[]}(e).map((e=>{let{props:{value:t,label:a,attributes:n,default:r}}=e;return{value:t,label:a,attributes:n,default:r}}))}function p(e){const{values:t,children:a}=e;return(0,r.useMemo)((()=>{const e=t??u(a);return function(e){const t=(0,m.l)(e,((e,t)=>e.value===t.value));if(t.length>0)throw new Error(`Docusaurus error: Duplicate values "${t.map((e=>e.value)).join(", ")}" found in <Tabs>. Every value needs to be unique.`)}(e),e}),[t,a])}function c(e){let{value:t,tabValues:a}=e;return a.some((e=>e.value===t))}function k(e){let{queryString:t=!1,groupId:a}=e;const n=(0,s.k6)(),l=function(e){let{queryString:t=!1,groupId:a}=e;if("string"==typeof t)return t;if(!1===t)return null;if(!0===t&&!a)throw new Error('Docusaurus error: The <Tabs> component groupId prop is required if queryString=true, because this value is used as the search param name. You can also provide an explicit value such as queryString="my-search-param".');return a??null}({queryString:t,groupId:a});return[(0,o._X)(l),(0,r.useCallback)((e=>{if(!l)return;const t=new URLSearchParams(n.location.search);t.set(l,e),n.replace({...n.location,search:t.toString()})}),[l,n])]}function g(e){const{defaultValue:t,queryString:a=!1,groupId:n}=e,l=p(e),[i,s]=(0,r.useState)((()=>function(e){let{defaultValue:t,tabValues:a}=e;if(0===a.length)throw new Error("Docusaurus error: the <Tabs> component requires at least one <TabItem> children component");if(t){if(!c({value:t,tabValues:a}))throw new Error(`Docusaurus error: The <Tabs> has a defaultValue "${t}" but none of its children has the corresponding value. Available values are: ${a.map((e=>e.value)).join(", ")}. If you intend to show no default tab, use defaultValue={null} instead.`);return t}const n=a.find((e=>e.default))??a[0];if(!n)throw new Error("Unexpected error: 0 tabValues");return n.value}({defaultValue:t,tabValues:l}))),[o,m]=k({queryString:a,groupId:n}),[u,g]=function(e){let{groupId:t}=e;const a=function(e){return e?`docusaurus.tab.${e}`:null}(t),[n,l]=(0,d.Nk)(a);return[n,(0,r.useCallback)((e=>{a&&l.set(e)}),[a,l])]}({groupId:n}),f=(()=>{const e=o??u;return c({value:e,tabValues:l})?e:null})();(0,r.useLayoutEffect)((()=>{f&&s(f)}),[f]);return{selectedValue:i,selectValue:(0,r.useCallback)((e=>{if(!c({value:e,tabValues:l}))throw new Error(`Can't select invalid tab value=${e}`);s(e),m(e),g(e)}),[m,g,l]),tabValues:l}}var f=a(72389);const h={tabList:"tabList__CuJ",tabItem:"tabItem_LNqP"};function N(e){let{className:t,block:a,selectedValue:s,selectValue:o,tabValues:m}=e;const d=[],{blockElementScrollPositionUntilNextRender:u}=(0,i.o5)(),p=e=>{const t=e.currentTarget,a=d.indexOf(t),n=m[a].value;n!==s&&(u(t),o(n))},c=e=>{let t=null;switch(e.key){case"Enter":p(e);break;case"ArrowRight":{const a=d.indexOf(e.currentTarget)+1;t=d[a]??d[0];break}case"ArrowLeft":{const a=d.indexOf(e.currentTarget)-1;t=d[a]??d[d.length-1];break}}t?.focus()};return r.createElement("ul",{role:"tablist","aria-orientation":"horizontal",className:(0,l.Z)("tabs",{"tabs--block":a},t)},m.map((e=>{let{value:t,label:a,attributes:i}=e;return r.createElement("li",(0,n.Z)({role:"tab",tabIndex:s===t?0:-1,"aria-selected":s===t,key:t,ref:e=>d.push(e),onKeyDown:c,onClick:p},i,{className:(0,l.Z)("tabs__item",h.tabItem,i?.className,{"tabs__item--active":s===t})}),a??t)})))}function y(e){let{lazy:t,children:a,selectedValue:n}=e;const l=(Array.isArray(a)?a:[a]).filter(Boolean);if(t){const e=l.find((e=>e.props.value===n));return e?(0,r.cloneElement)(e,{className:"margin-top--md"}):null}return r.createElement("div",{className:"margin-top--md"},l.map(((e,t)=>(0,r.cloneElement)(e,{key:t,hidden:e.props.value!==n}))))}function b(e){const t=g(e);return r.createElement("div",{className:(0,l.Z)("tabs-container",h.tabList)},r.createElement(N,(0,n.Z)({},e,t)),r.createElement(y,(0,n.Z)({},e,t)))}function v(e){const t=(0,f.Z)();return r.createElement(b,(0,n.Z)({key:String(t)},e))}},48796:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>d,contentTitle:()=>o,default:()=>k,frontMatter:()=>s,metadata:()=>m,toc:()=>u});var n=a(87462),r=(a(67294),a(3905)),l=a(74866),i=a(85162);const s={},o="Infer schemas",m={unversionedId:"user-guide/infer-schema",id:"version-1.0.0/user-guide/infer-schema",title:"Infer schemas",description:"In this section you will learn how to describe the samples data created when bootstrapping a new project.",source:"@site/versioned_docs/version-1.0.0/0400-user-guide/055.infer-schema.mdx",sourceDirName:"0400-user-guide",slug:"/user-guide/infer-schema",permalink:"/starlake/docs/user-guide/infer-schema",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/versioned_docs/version-1.0.0/0400-user-guide/055.infer-schema.mdx",tags:[],version:"1.0.0",sidebarPosition:55,frontMatter:{},sidebar:"starlakeSidebar",previous:{title:"Bootstrap project",permalink:"/starlake/docs/user-guide/bootstrap"},next:{title:"Extract",permalink:"/starlake/docs/user-guide/extract"}},d={},u=[{value:"Sample scenario",id:"sample-scenario",level:2},{value:"Sample datasets",id:"sample-datasets",level:4},{value:"Infer the schemas",id:"infer-the-schemas",level:2},{value:"Domain configuration",id:"domain-configuration",level:3},{value:"Table configuration",id:"table-configuration",level:3},{value:"Environment variables",id:"environment-variables",level:2},{value:"Next steps",id:"next-steps",level:2}],p={toc:u},c="wrapper";function k(e){let{components:t,...a}=e;return(0,r.kt)(c,(0,n.Z)({},p,a,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"infer-schemas"},"Infer schemas"),(0,r.kt)("p",null,"In this section you will learn how to describe the samples data created when ",(0,r.kt)("a",{parentName:"p",href:"bootstrap"},"bootstrapping a new project"),".\nWe will use the ",(0,r.kt)("inlineCode",{parentName:"p"},"infer-schema")," command to infer the schema of the sample data.\nThe resulting inferred schemas are the backbone of the data loading process and will prevent your datawarehouse from becoming a dataswamp."),(0,r.kt)("h2",{id:"sample-scenario"},"Sample scenario"),(0,r.kt)("p",null,"This userguide comes with 4 different sample files:  CSV, JSON and JSONL files."),(0,r.kt)("p",null,'The customers and orders are provided by the "sales" department as delimited separated values files\nand have to be loaded incrementally.'),(0,r.kt)("p",null,'The store locations and sellers are provided by the "hr" department as respectively JSONL and JSON files\nand have to be loaded in full load mode.'),(0,r.kt)("h4",{id:"sample-datasets"},"Sample datasets"),(0,r.kt)(l.Z,{groupId:"files",mdxType:"Tabs"},(0,r.kt)(i.Z,{value:"sales/customers",label:"customers",mdxType:"TabItem"},(0,r.kt)("p",null,(0,r.kt)("inlineCode",{parentName:"p"},'File customers-2018-01-01.psv from "sales" department')),(0,r.kt)("table",null,(0,r.kt)("thead",{parentName:"table"},(0,r.kt)("tr",{parentName:"thead"},(0,r.kt)("th",{parentName:"tr",align:null},"id"),(0,r.kt)("th",{parentName:"tr",align:null},"signup"),(0,r.kt)("th",{parentName:"tr",align:null},"contact"),(0,r.kt)("th",{parentName:"tr",align:null},"birthdate"),(0,r.kt)("th",{parentName:"tr",align:null},"firstname"),(0,r.kt)("th",{parentName:"tr",align:null},"lastname"),(0,r.kt)("th",{parentName:"tr",align:null},"country"))),(0,r.kt)("tbody",{parentName:"table"},(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"A009701"),(0,r.kt)("td",{parentName:"tr",align:null},"2010-01-31 23:04:15"),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("a",{parentName:"td",href:"mailto:k@m.com"},"k@m.com")),(0,r.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,r.kt)("td",{parentName:"tr",align:null},"Kylian"),(0,r.kt)("td",{parentName:"tr",align:null},"Mbapp\xe9"),(0,r.kt)("td",{parentName:"tr",align:null},"France")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"B000001"),(0,r.kt)("td",{parentName:"tr",align:null},"2016-12-01 09:56:02"),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("a",{parentName:"td",href:"mailto:n@b.com"},"n@b.com")),(0,r.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,r.kt)("td",{parentName:"tr",align:null},"Napoleon"),(0,r.kt)("td",{parentName:"tr",align:null},"Bonaparte"),(0,r.kt)("td",{parentName:"tr",align:null},"France")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"B000001"),(0,r.kt)("td",{parentName:"tr",align:null},"2016-12-02 09:56:02"),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("a",{parentName:"td",href:"mailto:m@c.com"},"m@c.com")),(0,r.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,r.kt)("td",{parentName:"tr",align:null},"Marie"),(0,r.kt)("td",{parentName:"tr",align:null},"Curie"),(0,r.kt)("td",{parentName:"tr",align:null},"France")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"B000002"),(0,r.kt)("td",{parentName:"tr",align:null},"2016-12-02 09:56:02"),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("a",{parentName:"td",href:"mailto:z@z.com"},"z@z.com")),(0,r.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,r.kt)("td",{parentName:"tr",align:null},"Zinedine"),(0,r.kt)("td",{parentName:"tr",align:null},"Zidane"),(0,r.kt)("td",{parentName:"tr",align:null},"France")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"B000003"),(0,r.kt)("td",{parentName:"tr",align:null},"2016-12-03 09:56:02"),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("a",{parentName:"td",href:"mailto:e@g.com"},"e@g.com")),(0,r.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,r.kt)("td",{parentName:"tr",align:null},"Eva"),(0,r.kt)("td",{parentName:"tr",align:null},"Green"),(0,r.kt)("td",{parentName:"tr",align:null},"France")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"B000012"),(0,r.kt)("td",{parentName:"tr",align:null},"2016-12-03 09:56:02"),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("a",{parentName:"td",href:"mailto:k@b.com"},"k@b.com")),(0,r.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,r.kt)("td",{parentName:"tr",align:null},"Karim"),(0,r.kt)("td",{parentName:"tr",align:null},"Benzema"),(0,r.kt)("td",{parentName:"tr",align:null},"France")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"B000004"),(0,r.kt)("td",{parentName:"tr",align:null},"2016-12-04 09:56:02"),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("a",{parentName:"td",href:"mailto:m@c.com"},"m@c.com")),(0,r.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,r.kt)("td",{parentName:"tr",align:null},"Marion"),(0,r.kt)("td",{parentName:"tr",align:null},"Cotillard"),(0,r.kt)("td",{parentName:"tr",align:null},"France")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"B000005"),(0,r.kt)("td",{parentName:"tr",align:null},"2016-12-05 09:56:02"),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("a",{parentName:"td",href:"mailto:a@g.com"},"a@g.com")),(0,r.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,r.kt)("td",{parentName:"tr",align:null},"Ariana"),(0,r.kt)("td",{parentName:"tr",align:null},"Grande"),(0,r.kt)("td",{parentName:"tr",align:null},"USA")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"B000006"),(0,r.kt)("td",{parentName:"tr",align:null},"2016-12-06 09:56:02"),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("a",{parentName:"td",href:"mailto:m@j.com"},"m@j.com")),(0,r.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,r.kt)("td",{parentName:"tr",align:null},"Michael"),(0,r.kt)("td",{parentName:"tr",align:null},"Jordan"),(0,r.kt)("td",{parentName:"tr",align:null},"USA")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"B000007"),(0,r.kt)("td",{parentName:"tr",align:null},"2016-12-07 09:56:02"),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("a",{parentName:"td",href:"mailto:m@a.com"},"m@a.com")),(0,r.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,r.kt)("td",{parentName:"tr",align:null},"Muhammad"),(0,r.kt)("td",{parentName:"tr",align:null},"Ali"),(0,r.kt)("td",{parentName:"tr",align:null},"USA")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"B000008"),(0,r.kt)("td",{parentName:"tr",align:null},"2016-12-08 09:56:02"),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("a",{parentName:"td",href:"mailto:t@s.com"},"t@s.com")),(0,r.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,r.kt)("td",{parentName:"tr",align:null},"Taylor"),(0,r.kt)("td",{parentName:"tr",align:null},"Swift"),(0,r.kt)("td",{parentName:"tr",align:null},"USA")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"B000009"),(0,r.kt)("td",{parentName:"tr",align:null},"2016-12-09 09:56:02"),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("a",{parentName:"td",href:"mailto:e@p.com"},"e@p.com")),(0,r.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,r.kt)("td",{parentName:"tr",align:null},"Elvis"),(0,r.kt)("td",{parentName:"tr",align:null},"Presley"),(0,r.kt)("td",{parentName:"tr",align:null},"USA")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"B000010"),(0,r.kt)("td",{parentName:"tr",align:null},"2016-12-10 09:56:02"),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("a",{parentName:"td",href:"mailto:s@j.com"},"s@j.com")),(0,r.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,r.kt)("td",{parentName:"tr",align:null},"Steve"),(0,r.kt)("td",{parentName:"tr",align:null},"Jobs"),(0,r.kt)("td",{parentName:"tr",align:null},"USA")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"B000011"),(0,r.kt)("td",{parentName:"tr",align:null},"2016-12-11 09:56:02"),(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("a",{parentName:"td",href:"mailto:a@l.com"},"a@l.com")),(0,r.kt)("td",{parentName:"tr",align:null},"1980-04-15"),(0,r.kt)("td",{parentName:"tr",align:null},"Abraham"),(0,r.kt)("td",{parentName:"tr",align:null},"Lincoln"),(0,r.kt)("td",{parentName:"tr",align:null},"USA"))))),(0,r.kt)(i.Z,{value:"sales/orders",label:"orders",mdxType:"TabItem"},(0,r.kt)("p",null,(0,r.kt)("inlineCode",{parentName:"p"},'File orders-2018-01-01.psv from "sales" department')),(0,r.kt)("table",null,(0,r.kt)("thead",{parentName:"table"},(0,r.kt)("tr",{parentName:"thead"},(0,r.kt)("th",{parentName:"tr",align:null},"order_id"),(0,r.kt)("th",{parentName:"tr",align:null},"customer_id"),(0,r.kt)("th",{parentName:"tr",align:null},"amount"),(0,r.kt)("th",{parentName:"tr",align:null},"seller_id"))),(0,r.kt)("tbody",{parentName:"table"},(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"12345"),(0,r.kt)("td",{parentName:"tr",align:null},"A009701"),(0,r.kt)("td",{parentName:"tr",align:null},"123.65"),(0,r.kt)("td",{parentName:"tr",align:null},"AQZERD")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"56432"),(0,r.kt)("td",{parentName:"tr",align:null},"A009701"),(0,r.kt)("td",{parentName:"tr",align:null},"23.8"),(0,r.kt)("td",{parentName:"tr",align:null},"AQZERD")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},"10000"),(0,r.kt)("td",{parentName:"tr",align:null},"B308629"),(0,r.kt)("td",{parentName:"tr",align:null},"23.8"),(0,r.kt)("td",{parentName:"tr",align:null},"AQZERD"))))),(0,r.kt)(i.Z,{value:"hr/sellers",label:"sellers",mdxType:"TabItem"},(0,r.kt)("p",null,(0,r.kt)("inlineCode",{parentName:"p"},'File sellers-2018-01-01.psv from "hr" department')),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-json"},'[\n    { "id":"AQZERD", "seller_email":"me@acme.com", "location_id": 1},\n    { "id":"TYUEZG", "seller_email":"acme.com","location_id": 2 }\n]\n'))),(0,r.kt)(i.Z,{value:"hr/locations",label:"locations",mdxType:"TabItem"},(0,r.kt)("p",null,(0,r.kt)("inlineCode",{parentName:"p"},'File locations-2018-01-01.psv from "hr" department')),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-json"},'{  "id":"1", "address": { "city":"Paris", "stores": ["Store 1", "Store 2", "Store 3"], "country":"France"} }\n{  "id":"2",  "address": {    "city":"Berlin",    "country":"Germany"  }}\n')))),(0,r.kt)("h2",{id:"infer-the-schemas"},"Infer the schemas"),(0,r.kt)("p",null,"In Starlake terms, after loading, we will end up with:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"one domain: ",(0,r.kt)("inlineCode",{parentName:"li"},"sales")," . A domain is equivalent to a database schema or a BigQuery dataset."),(0,r.kt)("li",{parentName:"ul"},"one table: the ",(0,r.kt)("inlineCode",{parentName:"li"},"customers")," table in the ",(0,r.kt)("inlineCode",{parentName:"li"},"sales")," domain")),(0,r.kt)("p",null,"We first need to write a YAML configuration file that describe the structure of the file to load into the warehouse.\nInstead of writing this file by hand, we may infer his YAML configuration file using the ",(0,r.kt)("inlineCode",{parentName:"p"},"infer-schema")," command."),(0,r.kt)(l.Z,{groupId:"platforms",mdxType:"Tabs"},(0,r.kt)(i.Z,{value:"linux_macos",label:"Linux/MacOS",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-sh"},"$ cd $HOME/userguide\n\n$ starlake infer-schema                                 \\\n    --domain sales                                      \\\n    --table customers                                   \\\n    --input sample-data/sales/customers-2018-01-01.psv  \\\n    --write APPEND                                      \\\n    --with-header\n\n$ starlake infer-schema                                 \\\n    --domain sales                                      \\\n    --table orders                                      \\\n    --input sample-data/sales/orders-2018-01-01.csv     \\\n    --write APPEND                                      \\\n    --with-header\n\n$ starlake infer-schema                                 \\\n    --domain hr                                         \\\n    --table locations                                   \\\n    --input sample-data/hr/locations-2018-01-01.json    \\\n    --write OVERWRITE                                   \\\n    --with-header\n\n$ starlake infer-schema                                 \\\n    --domain hr                                         \\\n    --table sellers                                     \\\n    --input sample-data/hr/sellers-2018-01-01.json      \\\n    --write OVERWRITE                                   \\\n    --with-header\n"))),(0,r.kt)(i.Z,{value:"windows",label:"Windows",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-powershell"},"c:\\users\\me\\userguide> starlake infer-schema                           ^\n    --domain sales                                                      ^\n    --table customers                                                   ^\n    --input sample-data/sales/customers-2018-01-01.psv                  ^\n    --write APPEND                                                     ^\n    --with-header\n"))),(0,r.kt)(i.Z,{value:"docker",label:"Docker",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-shell"},"$ docker run                                                        \\\n    -v $HOME/userguide:/app/userguide                             \\\n    -e SL_ROOT=/app/userguide -it starlake infer-schema            \\\n    --domain sales                                                  \\\n    --table customers                                               \\\n    --input /app/userguide/incoming/customers-2018-01-01.psv       \\\n    --write APPEND                                                 \\\n    --with-header\n")))),(0,r.kt)("p",null,"This inferred YAML file may be found in the ",(0,r.kt)("inlineCode",{parentName:"p"},"customers.sl.yml")," file under the ",(0,r.kt)("inlineCode",{parentName:"p"},"metadata/load/sales")," folder.\nNotice how the ",(0,r.kt)("inlineCode",{parentName:"p"},"customers.sl.yml")," file is named after the ",(0,r.kt)("em",{parentName:"p"},"table")," name and stored in the folder named after the ",(0,r.kt)("em",{parentName:"p"},"domain")," name.\nThe domain configuration file _",(0,r.kt)("em",{parentName:"p"},"config.sl.yml")," is also stored in the ",(0,r.kt)("em",{parentName:"p"},"domain")," folder."),(0,r.kt)("p",null,"The contents of the files look like this:"),(0,r.kt)("h3",{id:"domain-configuration"},"Domain configuration"),(0,r.kt)("p",null,"The  _",(0,r.kt)("em",{parentName:"p"},"config.sl.yml")," file describes the properties shared by all tables in this domain.\nHere we assume that all tables in the ",(0,r.kt)("inlineCode",{parentName:"p"},"sales")," domain are loaded from the ",(0,r.kt)("em",{parentName:"p"},"incoming"),"/sales folder and\nall tables in the ",(0,r.kt)("inlineCode",{parentName:"p"},"hr")," domain are loaded from the ",(0,r.kt)("em",{parentName:"p"},"incoming"),"/hr folder."),(0,r.kt)("p",null,(0,r.kt)("inlineCode",{parentName:"p"},"{{incoming_path}}")," is a variable path set in the env file (more on this later)."),(0,r.kt)(l.Z,{groupId:"domain_schemas",mdxType:"Tabs"},(0,r.kt)(i.Z,{value:"sales",label:"sales",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="Sales Domain: _config.sl.yml"',title:'"Sales',"Domain:":!0,'_config.sl.yml"':!0},'---\nload:\n    metadata:\n        directory: "{{incoming_path}}/sales"\n'))),(0,r.kt)(i.Z,{value:"hr",label:"hr",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="Sales Domain: _config.sl.yml"',title:'"Sales',"Domain:":!0,'_config.sl.yml"':!0},'---\nload:\n    metadata:\n        directory: "{{incoming_path}}/hr"\n')))),(0,r.kt)("p",null,"You may change the path referenced by the ",(0,r.kt)("em",{parentName:"p"},"directory")," attribute to any other path.\nFor example, you may want to load the data from a Google Cloud Storage bucket.\nthe generated file contains the ",(0,r.kt)("em",{parentName:"p"},"incoming_path")," variable that will be replaced at runtime by the value defined in the ",(0,r.kt)("a",{parentName:"p",href:"#environment-variables"},"environment file"),"."),(0,r.kt)("h3",{id:"table-configuration"},"Table configuration"),(0,r.kt)("p",null,"Let's Looking at one of the inferred schemas, the customers schema for exempole. The ",(0,r.kt)("em",{parentName:"p"},"customers.sl.yml")," YAML file describes\nthe structure of the file to load into the warehouse.\nThe ",(0,r.kt)("inlineCode",{parentName:"p"},"pattern")," property is a regular expression that will be used to match the file name."),(0,r.kt)(l.Z,{groupId:"table_schemas",mdxType:"Tabs"},(0,r.kt)(i.Z,{value:"sales/customers",label:"sales/customers",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="metadata/load/sales/customers.sl.yml"',title:'"metadata/load/sales/customers.sl.yml"'},'---\ntable:\n  pattern: "customers.*.psv" # This property is a regular expression that will be used to match the file name.\n                             # Please replace it by the adequate file pattern eq. customers-.*.psv if required\n  attributes:         # Description of the fields to recognize\n    - name: "id"        # attribute name and column name in the destination table if no rename attribute is defined\n      type: "string"    # expected type\n      array: false      # is it an array (false by default, ignored in DSV files) ?\n      required: false   # Is this field required in the source (false by default, change it accordingly) ?\n      privacy: "NONE"   # Should we encrypt this field before loading to the warehouse (No encryption by default )?\n      ignore: false     # Should this field be excluded (false by default) ?\n    - name: "signup"    # second attribute\n      type: "timestamp" # recognized type by analyzing input.\n    - name: "contact"\n      type: "string"\n      # ...\n    - name: "birthdate"\n      type: "date"      # recognized as semantic type date.\n      # ...\n    - name: "firstname"\n      type: "string"\n      # ...\n    - name: "lastname"\n      type: "string"\n      # ...\n    - name: "country"\n      type: "string"\n      # ...               # and so on ...\n  metadata:\n    mode: "FILE"\n    format: "DSV"         # detected format\n    encoding: "UTF-8"\n    multiline: false\n    array: false\n    withHeader: true\n    separator: "|"        # detected separator\n    quote: "\\""\n    escape: "\\\\"\n    write: "APPEND"\n\n'))),(0,r.kt)(i.Z,{value:"sales/orders",label:"sales/orders",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="metadata/load/sales/orders.sl.yml"',title:'"metadata/load/sales/orders.sl.yml"'},'---\ntable:\n  name: "orders"\n  pattern: "orders.*.csv"\n  attributes:\n  - name: "order_id"\n    type: "integer"\n    array: false\n    required: false\n    privacy: "NONE"\n    ignore: false\n  - name: "customer_id"\n    type: "string"\n    array: false\n    required: false\n    privacy: "NONE"\n    ignore: false\n  - name: "amount"\n    type: "double"\n    array: false\n    required: false\n    privacy: "NONE"\n    ignore: false\n  - name: "seller_id"\n    type: "string"\n    array: false\n    required: false\n    privacy: "NONE"\n    ignore: false\n  metadata:\n    mode: "FILE"\n    format: "DSV"\n    encoding: "UTF-8"\n    multiline: false\n    array: false\n    withHeader: true\n    separator: ","\n    quote: "\\""\n    escape: "\\\\"\n    write: "APPEND"\n\n'))),(0,r.kt)(i.Z,{value:"hr/sellers",label:"hr/sellers",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="metadata/load/hr/sellers.sl.yml"',title:'"metadata/load/hr/sellers.sl.yml"'},'---\ntable:\n  name: "sellers"\n  pattern: "sellers.*.json"\n  attributes:\n  - name: "id"\n    type: "string"\n    array: false\n    required: false\n    privacy: "NONE"\n    ignore: false\n  - name: "location_id"\n    type: "long"\n    array: false\n    required: false\n    privacy: "NONE"\n    ignore: false\n  - name: "seller_email"\n    type: "string"\n    array: false\n    required: false\n    privacy: "NONE"\n    ignore: false\n  metadata:\n    mode: "FILE"\n    format: "JSON"              # detected format:  JSON (array property set to true)\n    encoding: "UTF-8"\n    multiline: false\n    array: true\n    withHeader: true\n    separator: ";"\n    quote: "\\""\n    escape: "\\\\"\n    write: "OVERWRITE"\n\n\n'))),(0,r.kt)(i.Z,{value:"hr/locations",label:"hr/locations",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="metadata/load/hr/locations.sl.yml"',title:'"metadata/load/hr/locations.sl.yml"'},'---\ntable:\n  name: "locations"\n  pattern: "locations.*.json"\n  attributes:\n  - name: "address"\n    type: "struct"\n    array: false\n    required: false\n    privacy: "NONE"\n    attributes:\n    - name: "city"\n      type: "string"\n      array: false\n      required: false\n      privacy: "NONE"\n      ignore: false\n    - name: "country"\n      type: "string"\n      array: false\n      required: false\n      privacy: "NONE"\n      ignore: false\n    - name: "stores"\n      type: "string"\n      array: true\n      required: false\n      privacy: "NONE"\n      ignore: false\n    ignore: false\n  - name: "id"\n    type: "string"\n    array: false\n    required: false\n    privacy: "NONE"\n    ignore: false\n  metadata:\n    mode: "FILE"\n    format: "JSON"        # detected format:  JSONL (array property set to false)\n    encoding: "UTF-8"\n    multiline: false\n    array: false             \n    withHeader: true\n    separator: ";"\n    quote: "\\""\n    escape: "\\\\"\n    write: "OVERWRITE"\n\n')))),(0,r.kt)("p",null,"The generated files are located in the ",(0,r.kt)("inlineCode",{parentName:"p"},"metadata/load")," folder."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-text",metastring:"{4-12}","{4-12}":!0},"metadata\n\u251c\u2500\u2500 application.sl.yml\n\u251c\u2500\u2500 env.sl.yml\n\u251c\u2500\u2500 load\n\u2502   \u251c\u2500\u2500 hr\n\u2502   \u2502   \u251c\u2500\u2500 _config.sl.yml\n\u2502   \u2502   \u251c\u2500\u2500 locations.sl.yml\n\u2502   \u2502   \u2514\u2500\u2500 sellers.sl.yml\n\u2502   \u2514\u2500\u2500 sales\n\u2502       \u251c\u2500\u2500 _config.sl.yml\n\u2502       \u251c\u2500\u2500 customers.sl.yml\n\u2502       \u2514\u2500\u2500 orders.sl.yml\n\u2514\u2500\u2500 types\n    \u251c\u2500\u2500 default.sl.yml\n    \u2514\u2500\u2500 types.sl.yml\n")),(0,r.kt)("h2",{id:"environment-variables"},"Environment variables"),(0,r.kt)("p",null,"The environment file ",(0,r.kt)("inlineCode",{parentName:"p"},"metadata/env.sl.yml")," define your project variables."),(0,r.kt)("p",null,"In our example, the ",(0,r.kt)("inlineCode",{parentName:"p"},"incoming_path")," must be defined to the location where the incoming directory containing the datasets to load will be located.\nSet it to your project sample-data sub-folder as follows in the default environment file:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},'env:\n  incoming_path: "{{SL_ROOT}}/sample-data"\n')),(0,r.kt)("p",null,(0,r.kt)("inlineCode",{parentName:"p"},"SL_ROOT")," is a special variable that is set to the directory from where the ",(0,r.kt)("inlineCode",{parentName:"p"},"starlake")," command is executed, usually the project directory."),(0,r.kt)("h2",{id:"next-steps"},"Next steps"))}k.isMDXComponent=!0}}]);