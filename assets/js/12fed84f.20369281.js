"use strict";(self.webpackChunkstarlake_docs=self.webpackChunkstarlake_docs||[]).push([[1065],{3905:(e,t,r)=>{r.d(t,{Zo:()=>u,kt:()=>f});var n=r(67294);function a(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function o(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function i(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?o(Object(r),!0).forEach((function(t){a(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):o(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function s(e,t){if(null==e)return{};var r,n,a=function(e,t){if(null==e)return{};var r,n,a={},o=Object.keys(e);for(n=0;n<o.length;n++)r=o[n],t.indexOf(r)>=0||(a[r]=e[r]);return a}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(n=0;n<o.length;n++)r=o[n],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(a[r]=e[r])}return a}var c=n.createContext({}),l=function(e){var t=n.useContext(c),r=t;return e&&(r="function"==typeof e?e(t):i(i({},t),e)),r},u=function(e){var t=l(e.components);return n.createElement(c.Provider,{value:t},e.children)},d="mdxType",m={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},p=n.forwardRef((function(e,t){var r=e.components,a=e.mdxType,o=e.originalType,c=e.parentName,u=s(e,["components","mdxType","originalType","parentName"]),d=l(r),p=a,f=d["".concat(c,".").concat(p)]||d[p]||m[p]||o;return r?n.createElement(f,i(i({ref:t},u),{},{components:r})):n.createElement(f,i({ref:t},u))}));function f(e,t){var r=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var o=r.length,i=new Array(o);i[0]=p;var s={};for(var c in t)hasOwnProperty.call(t,c)&&(s[c]=t[c]);s.originalType=e,s[d]="string"==typeof e?e:a,i[1]=s;for(var l=2;l<o;l++)i[l]=r[l];return n.createElement.apply(null,i)}return n.createElement.apply(null,r)}p.displayName="MDXCreateElement"},89345:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>c,contentTitle:()=>i,default:()=>m,frontMatter:()=>o,metadata:()=>s,toc:()=>l});var n=r(87462),a=(r(67294),r(3905));const o={},i="Quickstart",s={unversionedId:"quickstart/starbake",id:"quickstart/starbake",title:"Quickstart",description:"Starbake Commerce",source:"@site/docs/030-quickstart/010-starbake.mdx",sourceDirName:"030-quickstart",slug:"/quickstart/starbake",permalink:"/starlake/docs/next/quickstart/starbake",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/docs/030-quickstart/010-starbake.mdx",tags:[],version:"current",sidebarPosition:10,frontMatter:{},sidebar:"starlakeSidebar",previous:{title:"Quickstart",permalink:"/starlake/docs/next/category/quickstart"},next:{title:"User guide",permalink:"/starlake/docs/next/category/user-guide"}},c={},l=[{value:"Starbake Commerce",id:"starbake-commerce",level:2},{value:"Download",id:"download",level:2}],u={toc:l},d="wrapper";function m(e){let{components:t,...r}=e;return(0,a.kt)(d,(0,n.Z)({},u,r,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("h1",{id:"quickstart"},"Quickstart"),(0,a.kt)("h2",{id:"starbake-commerce"},"Starbake Commerce"),(0,a.kt)("p",null,"To illustrate Starlake features, we will use the StarBake github project.\nStarBake is a fictional & demonstrative project designed to showcase the usage of Starlake for data loading, transformation and analytics\nin an e-commerce bakery business setting."),(0,a.kt)("mermaid",{value:"classDiagram\n    Customers --|> CustomerLifetimeValue: uses\n    Orders --|> CustomerLifetimeValue: uses\n    Orders --|> ProductPerformance: uses\n    Products --|> ProductPerformance: uses\n    Products --|> ProductProfitability: uses\n    Ingredients --|> ProductProfitability: uses\n    CustomerLifetimeValue --|> HighValueCustomers: uses\n    ProductPerformance --|> TopSellingProducts: uses\n    ProductProfitability --|> MostProfitableProducts: uses\n    TopSellingProducts --|> TopSellingProfitableProducts: uses\n    MostProfitableProducts --|> TopSellingProfitableProducts: uses\n\n    class CustomerLifetimeValue {\n        +customer_id: integer\n        +customer_full_name: string\n        +customer_join_date: date\n        +total_spend_to_date: decimal\n        +average_spend_per_order: decimal\n        +frequency_of_orders: integer\n    }\n\n    class ProductPerformance {\n        +product_id: integer\n        +product_name: string\n        +total_units_sold: integer\n        +total_revenue: decimal\n        +average_revenue_per_unit: decimal\n    }\n\n    class ProductProfitability {\n        +product_id: integer\n        +product_name: string\n        +profit_margin_per_product: decimal\n    }\n\n    class HighValueCustomers {\n        +customer_id: integer\n        +customer_full_name: string\n        +lifetime_value: decimal\n    }\n\n    class TopSellingProducts {\n        +product_id: integer\n        +product_name: string\n        +units_sold: integer\n        +revenue: decimal\n    }\n\n    class MostProfitableProducts {\n        +product_id: integer\n        +product_name: string\n        +profit_margin: decimal\n    }\n\n    class TopSellingProfitableProducts {\n        +product_id: integer\n        +product_name: string\n        +units_sold: integer\n        +revenue: decimal\n        +profit_margin: decimal\n    }\n\n"}),(0,a.kt)("h2",{id:"download"},"Download"),(0,a.kt)("p",null,"The project begins by collecting raw operational data from various domains, including digital transactions, customer interactions, inventory management,\nand supplier relationships."),(0,a.kt)("p",null,"To clone the project and get started, run the following commands:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-bash"},"git clone https://github.com/starlake-ai/StarBake.git\ncd starbake\n")))}m.isMDXComponent=!0}}]);