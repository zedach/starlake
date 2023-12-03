"use strict";(self.webpackChunkstarlake=self.webpackChunkstarlake||[]).push([[5821],{3905:(t,e,a)=>{a.d(e,{Zo:()=>d,kt:()=>k});var r=a(67294);function n(t,e,a){return e in t?Object.defineProperty(t,e,{value:a,enumerable:!0,configurable:!0,writable:!0}):t[e]=a,t}function l(t,e){var a=Object.keys(t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(t);e&&(r=r.filter((function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable}))),a.push.apply(a,r)}return a}function i(t){for(var e=1;e<arguments.length;e++){var a=null!=arguments[e]?arguments[e]:{};e%2?l(Object(a),!0).forEach((function(e){n(t,e,a[e])})):Object.getOwnPropertyDescriptors?Object.defineProperties(t,Object.getOwnPropertyDescriptors(a)):l(Object(a)).forEach((function(e){Object.defineProperty(t,e,Object.getOwnPropertyDescriptor(a,e))}))}return t}function o(t,e){if(null==t)return{};var a,r,n=function(t,e){if(null==t)return{};var a,r,n={},l=Object.keys(t);for(r=0;r<l.length;r++)a=l[r],e.indexOf(a)>=0||(n[a]=t[a]);return n}(t,e);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(t);for(r=0;r<l.length;r++)a=l[r],e.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(t,a)&&(n[a]=t[a])}return n}var p=r.createContext({}),s=function(t){var e=r.useContext(p),a=e;return t&&(a="function"==typeof t?t(e):i(i({},e),t)),a},d=function(t){var e=s(t.components);return r.createElement(p.Provider,{value:e},t.children)},m="mdxType",f={inlineCode:"code",wrapper:function(t){var e=t.children;return r.createElement(r.Fragment,{},e)}},c=r.forwardRef((function(t,e){var a=t.components,n=t.mdxType,l=t.originalType,p=t.parentName,d=o(t,["components","mdxType","originalType","parentName"]),m=s(a),c=n,k=m["".concat(p,".").concat(c)]||m[c]||f[c]||l;return a?r.createElement(k,i(i({ref:e},d),{},{components:a})):r.createElement(k,i({ref:e},d))}));function k(t,e){var a=arguments,n=e&&e.mdxType;if("string"==typeof t||n){var l=a.length,i=new Array(l);i[0]=c;var o={};for(var p in e)hasOwnProperty.call(e,p)&&(o[p]=e[p]);o.originalType=t,o[m]="string"==typeof t?t:n,i[1]=o;for(var s=2;s<l;s++)i[s]=a[s];return r.createElement.apply(null,i)}return r.createElement.apply(null,a)}c.displayName="MDXCreateElement"},94454:(t,e,a)=>{a.r(e),a.d(e,{assets:()=>p,contentTitle:()=>i,default:()=>f,frontMatter:()=>l,metadata:()=>o,toc:()=>s});var r=a(87462),n=(a(67294),a(3905));const l={},i="Load",o={unversionedId:"configuration/load",id:"version-1.0.0/configuration/load",title:"Load",description:"Load Strategy",source:"@site/versioned_docs/version-1.0.0/0500-configuration/0130-load.mdx",sourceDirName:"0500-configuration",slug:"/configuration/load",permalink:"/starlake/docs/configuration/load",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/versioned_docs/version-1.0.0/0500-configuration/0130-load.mdx",tags:[],version:"1.0.0",sidebarPosition:130,frontMatter:{},sidebar:"starlakeSidebar",previous:{title:"Filesystem",permalink:"/starlake/docs/configuration/filesystem"},next:{title:"Platforms",permalink:"/starlake/docs/category/platforms"}},p={},s=[{value:"Load Strategy",id:"load-strategy",level:2},{value:"Validation",id:"validation",level:2},{value:"Privacy",id:"privacy",level:2}],d={toc:s},m="wrapper";function f(t){let{components:e,...a}=t;return(0,n.kt)(m,(0,r.Z)({},d,a,{components:e,mdxType:"MDXLayout"}),(0,n.kt)("h1",{id:"load"},"Load"),(0,n.kt)("h2",{id:"load-strategy"},"Load Strategy"),(0,n.kt)("p",null,"When many files that have the same pattern and thus belong to the same schema, it is possible to ingest them one after the other using an ingestion policy\nor ingest all of them at once."),(0,n.kt)("p",null,"When ingesting the files with the same schema one after the other, it is possible to use a custom ordering policy by settings the ",(0,n.kt)("inlineCode",{parentName:"p"},"SL_LOAD_STRATEGY")," environment variable. Currently, the following ordering policies are defined:"),(0,n.kt)("ul",null,(0,n.kt)("li",{parentName:"ul"},(0,n.kt)("inlineCode",{parentName:"li"},"ai.starlake.job.load.IngestionTimeStrategy")," : Order the files by modification date"),(0,n.kt)("li",{parentName:"ul"},(0,n.kt)("inlineCode",{parentName:"li"},"ai.starlake.job.load.IngestionNameStrategy")," : Order  the files by name")),(0,n.kt)("p",null,"If you want to use another custom strategy, you'll have to implement the trait below, make it available in the classpath and set the ",(0,n.kt)("inlineCode",{parentName:"p"},"SL_LOAD_STRATEGY")," environment variable"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-scala"},'package ai.starlake.job.load\n\nimport java.time.LocalDateTime\n\nimport org.apache.hadoop.fs.{FileSystem, Path}\n\ntrait LoadStrategy {\n\n  /** List all files in folder\n    *\n    * @param fs        FileSystem\n    * @param path      Absolute folder path\n    * @param extension Files should end with this string. To list all files, simply provide an empty string\n    * @param since     Minimum modification time of list files. To list all files, simply provide the beginning of all times\n    * @param recursive List files recursively\n    * @return List of Path\n    */\n  def list(\n    fs: FileSystem,\n    path: Path,\n    extension: String = "",\n    since: LocalDateTime = LocalDateTime.MIN,\n    recursive: Boolean\n  ): List[Path]\n}\n')),(0,n.kt)("p",null,"To ingest all the files at once, set the ",(0,n.kt)("inlineCode",{parentName:"p"},"SL_GROUPED")," variable to true."),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"YAML Variable"),(0,n.kt)("th",{parentName:"tr",align:"left"},"Env variable"),(0,n.kt)("th",{parentName:"tr",align:"left"},"Default Value"),(0,n.kt)("th",{parentName:"tr",align:"left"},"Description"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"grouped"),(0,n.kt)("td",{parentName:"tr",align:"left"},"SL_GROUPED"),(0,n.kt)("td",{parentName:"tr",align:"left"},"false"),(0,n.kt)("td",{parentName:"tr",align:"left"},"Should files with the same schema be ingested all at once ?")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"loadStrategyClass"),(0,n.kt)("td",{parentName:"tr",align:"left"},"SL_LOAD_STRATEGY"),(0,n.kt)("td",{parentName:"tr",align:"left"},"ai.starlake.job.load.IngestionTimeStrategy"),(0,n.kt)("td",{parentName:"tr",align:"left"},"When ",(0,n.kt)("inlineCode",{parentName:"td"},"grouped")," is false, which ingestion order strategy to use")))),(0,n.kt)("p",null,"Below is an example of YAML file with the default values."),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-yaml"},'application:\n  load-strategy-class: "ai.starlake.job.load.IngestionTimeStrategy" # may be overriden by the ${?SL_LOAD_STRATEGY} env variable\n  grouped: false # may be overriden by the ${?SL_GROUPED} env variable\n')),(0,n.kt)("h2",{id:"validation"},"Validation"),(0,n.kt)("p",null,"During ingestion, the input file is validated up to the attribute level. Three default row validators are defined:"),(0,n.kt)("ul",null,(0,n.kt)("li",{parentName:"ul"},"ai.starlake.job.validator.FlatRowValidator: to validate flat files, eq. DSV, Position and single level Json files."),(0,n.kt)("li",{parentName:"ul"},"ai.starlake.job.validator.TreeRowValidator:  used for tree like documents, eq. XML and JSON files"),(0,n.kt)("li",{parentName:"ul"},"ai.starlake.job.validator.AcceptAllValidator: used for any document type (flat and tree like) and accept the input without any validation")),(0,n.kt)("p",null,"The validtor to use is configurable as follows:"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"YAML Variable"),(0,n.kt)("th",{parentName:"tr",align:"left"},"Env. variable"),(0,n.kt)("th",{parentName:"tr",align:"left"},"Default value"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"rowValidatorClass"),(0,n.kt)("td",{parentName:"tr",align:"left"},"SL_ROW_VALIDATOR_CLASS"),(0,n.kt)("td",{parentName:"tr",align:"left"},"ai.starlake.job.validator.FlatRowValidator")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"treeValidatorClass"),(0,n.kt)("td",{parentName:"tr",align:"left"},"SL_TREE_VALIDATOR_CLASS"),(0,n.kt)("td",{parentName:"tr",align:"left"},"ai.starlake.job.validator.TreeRowValidator")))),(0,n.kt)("h2",{id:"privacy"},"Privacy"),(0,n.kt)("p",null,"Default valid values are NONE, HIDE, MD5, SHA1, SHA256, SHA512, AES(not implemented).\nCustom values may also be defined by adding a new privacy option in the application.conf.\nThe default reference.conf file defines the following valid privacy strategies:"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-hocon"},'privacy {\n  options = {\n    "none": "ai.starlake.privacy.No",\n    "hide": "ai.starlake.privacy.Hide",\n    "hide10X": "ai.starlake.privacy.Hide(\\"X\\",10)",\n    "approxLong20": "ai.starlake.privacy.ApproxLong(20)",\n    "md5": "ai.starlake.privacy.Md5",\n    "sha1": "ai.starlake.privacy.Sha1",\n    "sha256": "ai.starlake.privacy.Sha256",\n    "sha512": "ai.starlake.privacy.Sha512",\n    "initials": "ai.starlake.privacy.Initials"\n  }\n}\n')),(0,n.kt)("p",null,"In the YAML/HOCON file, reference, you reference the option name. This will apply the function defined in the class referenced by the option value."),(0,n.kt)("p",null,"Below the predefined strategies:"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Privacy Strategy"),(0,n.kt)("th",{parentName:"tr",align:"left"},"Privacy class"),(0,n.kt)("th",{parentName:"tr",align:"left"},"Description"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"none"),(0,n.kt)("td",{parentName:"tr",align:"left"},"ai.starlake.privacy.No"),(0,n.kt)("td",{parentName:"tr",align:"left"},"Return the input string itself")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"hide"),(0,n.kt)("td",{parentName:"tr",align:"left"},'ai.starlake.privacy.Hide(\\"X\\", 10)'),(0,n.kt)("td",{parentName:"tr",align:"left"},"Without a parameter, return the empty string. Otherwise, replace with 10 occurrences of the character 'X'")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"md5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"ai.starlake.privacy.Md5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"Return the md5 of the input string")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"sha1"),(0,n.kt)("td",{parentName:"tr",align:"left"},"ai.starlake.privacy.Sha1"),(0,n.kt)("td",{parentName:"tr",align:"left"},"Return the sha1 of the input string")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"sha256"),(0,n.kt)("td",{parentName:"tr",align:"left"},"ai.starlake.privacy.Sha256"),(0,n.kt)("td",{parentName:"tr",align:"left"},"Return the sha256 of the input string")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"sha512"),(0,n.kt)("td",{parentName:"tr",align:"left"},"ai.starlake.privacy.Sha512"),(0,n.kt)("td",{parentName:"tr",align:"left"},"Return the sha256 of the input string")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"initials"),(0,n.kt)("td",{parentName:"tr",align:"left"},"ai.starlake.privacy.Initials"),(0,n.kt)("td",{parentName:"tr",align:"left"},"Return the first char of each word (usually applied to user names)")))),(0,n.kt)("p",null,"The following startegies are also defined and may be declared in the custom configuration file."),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Privacy class"),(0,n.kt)("th",{parentName:"tr",align:"left"},"Description"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"ai.starlake.privacy.IPv4(8)"),(0,n.kt)("td",{parentName:"tr",align:"left"},"Return the IPv4 address with the last 8 bytes masked")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"ai.starlake.privacy.IPv6(8"),(0,n.kt)("td",{parentName:"tr",align:"left"},"Return the IPv6 address with the last 8 bytes masked")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"ai.starlake.privacy.RandomDouble"),(0,n.kt)("td",{parentName:"tr",align:"left"},"Return a random double number")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"ai.starlake.privacy.RandomDouble(10,20)"),(0,n.kt)("td",{parentName:"tr",align:"left"},"Return a random double between 10.0 and 20.0")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"ai.starlake.privacy.RandomLong"),(0,n.kt)("td",{parentName:"tr",align:"left"},"Return a random long number")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"ai.starlake.privacy.RandomLong(10, 20)"),(0,n.kt)("td",{parentName:"tr",align:"left"},"Return a random long number between 10 and 20")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"ai.starlake.privacy.RandomInt"),(0,n.kt)("td",{parentName:"tr",align:"left"},"Return a random int number")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"ai.starlake.privacy.RandomInt(10, 20)"),(0,n.kt)("td",{parentName:"tr",align:"left"},"Return a random int number between 10 and 20")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"ai.starlake.privacy.ApproxDouble(70)"),(0,n.kt)("td",{parentName:"tr",align:"left"},"Return a double value with a variation up to 70% applied to the input value")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"ai.starlake.privacy.ApproxLong(70)"),(0,n.kt)("td",{parentName:"tr",align:"left"},"Return a double long with a variation up to 70% applied to the input value")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},'ai.starlake.privacy.Mask(\\"*\\", 4, 1, 3)'),(0,n.kt)("td",{parentName:"tr",align:"left"},"Partially mask the input value with 4 occurrences of the '*' character, 1 on the left side and 3 on the right side.")))),(0,n.kt)("p",null,"Any new privacy strategy should implement the following trait :"),(0,n.kt)("pre",null,(0,n.kt)("code",{parentName:"pre",className:"language-scala"},"/** @param s: String  => Input string to encrypt\n  * @param colMap : Map[String, Option[String]] => Map of all the attributes and their corresponding values\n  * @param params: List[Any]  => Parameters passed to the algorithm as defined in the conf file.\n  *                               Parameter starting with '\"' is converted to a string\n  *                               Parameter containing a '.' is converted to a double\n  *                               Parameter equals to true of false is converted a boolean\n  *                               Anything else is converted to an int\n  * @return The encrypted string\n  */\n")))}f.isMDXComponent=!0}}]);