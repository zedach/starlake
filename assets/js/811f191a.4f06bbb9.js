"use strict";(self.webpackChunkstarlake_docs=self.webpackChunkstarlake_docs||[]).push([[616],{3905:(e,t,a)=>{a.d(t,{Zo:()=>m,kt:()=>f});var n=a(7294);function r(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function o(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function l(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?o(Object(a),!0).forEach((function(t){r(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):o(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function s(e,t){if(null==e)return{};var a,n,r=function(e,t){if(null==e)return{};var a,n,r={},o=Object.keys(e);for(n=0;n<o.length;n++)a=o[n],t.indexOf(a)>=0||(r[a]=e[a]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(n=0;n<o.length;n++)a=o[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(r[a]=e[a])}return r}var i=n.createContext({}),u=function(e){var t=n.useContext(i),a=t;return e&&(a="function"==typeof e?e(t):l(l({},t),e)),a},m=function(e){var t=u(e.components);return n.createElement(i.Provider,{value:t},e.children)},c="mdxType",p={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},d=n.forwardRef((function(e,t){var a=e.components,r=e.mdxType,o=e.originalType,i=e.parentName,m=s(e,["components","mdxType","originalType","parentName"]),c=u(a),d=r,f=c["".concat(i,".").concat(d)]||c[d]||p[d]||o;return a?n.createElement(f,l(l({ref:t},m),{},{components:a})):n.createElement(f,l({ref:t},m))}));function f(e,t){var a=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=a.length,l=new Array(o);l[0]=d;var s={};for(var i in t)hasOwnProperty.call(t,i)&&(s[i]=t[i]);s.originalType=e,s[c]="string"==typeof e?e:r,l[1]=s;for(var u=2;u<o;u++)l[u]=a[u];return n.createElement.apply(null,l)}return n.createElement.apply(null,a)}d.displayName="MDXCreateElement"},5162:(e,t,a)=>{a.d(t,{Z:()=>l});var n=a(7294),r=a(6010);const o={tabItem:"tabItem_Ymn6"};function l(e){let{children:t,hidden:a,className:l}=e;return n.createElement("div",{role:"tabpanel",className:(0,r.Z)(o.tabItem,l),hidden:a},t)}},4866:(e,t,a)=>{a.d(t,{Z:()=>v});var n=a(7462),r=a(7294),o=a(6010),l=a(2466),s=a(6550),i=a(1980),u=a(7392),m=a(12);function c(e){return function(e){return r.Children.map(e,(e=>{if(!e||(0,r.isValidElement)(e)&&function(e){const{props:t}=e;return!!t&&"object"==typeof t&&"value"in t}(e))return e;throw new Error(`Docusaurus error: Bad <Tabs> child <${"string"==typeof e.type?e.type:e.type.name}>: all children of the <Tabs> component should be <TabItem>, and every <TabItem> should have a unique "value" prop.`)}))?.filter(Boolean)??[]}(e).map((e=>{let{props:{value:t,label:a,attributes:n,default:r}}=e;return{value:t,label:a,attributes:n,default:r}}))}function p(e){const{values:t,children:a}=e;return(0,r.useMemo)((()=>{const e=t??c(a);return function(e){const t=(0,u.l)(e,((e,t)=>e.value===t.value));if(t.length>0)throw new Error(`Docusaurus error: Duplicate values "${t.map((e=>e.value)).join(", ")}" found in <Tabs>. Every value needs to be unique.`)}(e),e}),[t,a])}function d(e){let{value:t,tabValues:a}=e;return a.some((e=>e.value===t))}function f(e){let{queryString:t=!1,groupId:a}=e;const n=(0,s.k6)(),o=function(e){let{queryString:t=!1,groupId:a}=e;if("string"==typeof t)return t;if(!1===t)return null;if(!0===t&&!a)throw new Error('Docusaurus error: The <Tabs> component groupId prop is required if queryString=true, because this value is used as the search param name. You can also provide an explicit value such as queryString="my-search-param".');return a??null}({queryString:t,groupId:a});return[(0,i._X)(o),(0,r.useCallback)((e=>{if(!o)return;const t=new URLSearchParams(n.location.search);t.set(o,e),n.replace({...n.location,search:t.toString()})}),[o,n])]}function k(e){const{defaultValue:t,queryString:a=!1,groupId:n}=e,o=p(e),[l,s]=(0,r.useState)((()=>function(e){let{defaultValue:t,tabValues:a}=e;if(0===a.length)throw new Error("Docusaurus error: the <Tabs> component requires at least one <TabItem> children component");if(t){if(!d({value:t,tabValues:a}))throw new Error(`Docusaurus error: The <Tabs> has a defaultValue "${t}" but none of its children has the corresponding value. Available values are: ${a.map((e=>e.value)).join(", ")}. If you intend to show no default tab, use defaultValue={null} instead.`);return t}const n=a.find((e=>e.default))??a[0];if(!n)throw new Error("Unexpected error: 0 tabValues");return n.value}({defaultValue:t,tabValues:o}))),[i,u]=f({queryString:a,groupId:n}),[c,k]=function(e){let{groupId:t}=e;const a=function(e){return e?`docusaurus.tab.${e}`:null}(t),[n,o]=(0,m.Nk)(a);return[n,(0,r.useCallback)((e=>{a&&o.set(e)}),[a,o])]}({groupId:n}),h=(()=>{const e=i??c;return d({value:e,tabValues:o})?e:null})();(0,r.useLayoutEffect)((()=>{h&&s(h)}),[h]);return{selectedValue:l,selectValue:(0,r.useCallback)((e=>{if(!d({value:e,tabValues:o}))throw new Error(`Can't select invalid tab value=${e}`);s(e),u(e),k(e)}),[u,k,o]),tabValues:o}}var h=a(2389);const y={tabList:"tabList__CuJ",tabItem:"tabItem_LNqP"};function b(e){let{className:t,block:a,selectedValue:s,selectValue:i,tabValues:u}=e;const m=[],{blockElementScrollPositionUntilNextRender:c}=(0,l.o5)(),p=e=>{const t=e.currentTarget,a=m.indexOf(t),n=u[a].value;n!==s&&(c(t),i(n))},d=e=>{let t=null;switch(e.key){case"Enter":p(e);break;case"ArrowRight":{const a=m.indexOf(e.currentTarget)+1;t=m[a]??m[0];break}case"ArrowLeft":{const a=m.indexOf(e.currentTarget)-1;t=m[a]??m[m.length-1];break}}t?.focus()};return r.createElement("ul",{role:"tablist","aria-orientation":"horizontal",className:(0,o.Z)("tabs",{"tabs--block":a},t)},u.map((e=>{let{value:t,label:a,attributes:l}=e;return r.createElement("li",(0,n.Z)({role:"tab",tabIndex:s===t?0:-1,"aria-selected":s===t,key:t,ref:e=>m.push(e),onKeyDown:d,onClick:p},l,{className:(0,o.Z)("tabs__item",y.tabItem,l?.className,{"tabs__item--active":s===t})}),a??t)})))}function g(e){let{lazy:t,children:a,selectedValue:n}=e;const o=(Array.isArray(a)?a:[a]).filter(Boolean);if(t){const e=o.find((e=>e.props.value===n));return e?(0,r.cloneElement)(e,{className:"margin-top--md"}):null}return r.createElement("div",{className:"margin-top--md"},o.map(((e,t)=>(0,r.cloneElement)(e,{key:t,hidden:e.props.value!==n}))))}function w(e){const t=k(e);return r.createElement("div",{className:(0,o.Z)("tabs-container",y.tabList)},r.createElement(b,(0,n.Z)({},e,t)),r.createElement(g,(0,n.Z)({},e,t)))}function v(e){const t=(0,h.Z)();return r.createElement(w,(0,n.Z)({key:String(t)},e))}},590:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>m,contentTitle:()=>i,default:()=>f,frontMatter:()=>s,metadata:()=>u,toc:()=>c});var n=a(7462),r=(a(7294),a(3905)),o=a(4866),l=a(5162);const s={sidebar_position:150,title:"Transform"},i=void 0,u={unversionedId:"quickstart/transform",id:"version-0.8.0/quickstart/transform",title:"Transform",description:"Now that our file is successfully loaded and available as a table, we usually need to join them and expose KPIs.",source:"@site/versioned_docs/version-0.8.0/quickstart/150.transform.mdx",sourceDirName:"quickstart",slug:"/quickstart/transform",permalink:"/starlake/docs/0.8.0/quickstart/transform",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/versioned_docs/version-0.8.0/quickstart/150.transform.mdx",tags:[],version:"0.8.0",sidebarPosition:150,frontMatter:{sidebar_position:150,title:"Transform"},sidebar:"starlakeSidebar",previous:{title:"Load",permalink:"/starlake/docs/0.8.0/quickstart/load"},next:{title:"Complete example",permalink:"/starlake/docs/0.8.0/userguide/example"}},m={},c=[{value:"SQL Transforms",id:"sql-transforms",level:2},{value:"Jinja2 Transforms",id:"jinja2-transforms",level:2},{value:"Python/PySpark Transforms",id:"pythonpyspark-transforms",level:2},{value:"Targeting another datawarehouse",id:"targeting-another-datawarehouse",level:2}],p={toc:c},d="wrapper";function f(e){let{components:t,...a}=e;return(0,r.kt)(d,(0,n.Z)({},p,a,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("p",null,"Now that our file is successfully loaded and available as a table, we usually need to join them and expose KPIs.\nThis is where Starlake Transforms come into play.\nThree types of jobs are supported:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"SQL jobs: a single SQL file"),(0,r.kt)("li",{parentName:"ul"},"Jinja2 jobs: a single SQL/Python/PySpark file with Jinja2 templating"),(0,r.kt)("li",{parentName:"ul"},"Python jobs: a single Python/PySpark file")),(0,r.kt)("h2",{id:"sql-transforms"},"SQL Transforms"),(0,r.kt)("p",null,"We create a file ",(0,r.kt)("inlineCode",{parentName:"p"},"metadata/transform/kpi/customers_kpi.sql")," with the following content"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-SQL"},"select count(*) cnt from sales.customers\n")),(0,r.kt)("p",null,"This SQL file alone is sufficient to be a Starlake Transform. It instructs Starlake to put the result of the query in a schema (dataset in BigQuery)\nnamed after the folder name,  ",(0,r.kt)("inlineCode",{parentName:"p"},"kpi")," in our case and the table named after the sql filename aka ",(0,r.kt)("inlineCode",{parentName:"p"},"customers_kpi"),"."),(0,r.kt)("admonition",{type:"note"},(0,r.kt)("p",{parentName:"admonition"},"By default, the existing table data is overriden. To append data, use the ",(0,r.kt)("inlineCode",{parentName:"p"},"write")," option in the ",(0,r.kt)("inlineCode",{parentName:"p"},"metadata/transform/kpi/_config.sl.yml")," file:"),(0,r.kt)("pre",{parentName:"admonition"},(0,r.kt)("code",{parentName:"pre",className:"language-yaml"},"transform:\n  default:\n    write: APPEND\n")),(0,r.kt)("p",{parentName:"admonition"},"This instructs to run all transformations in ",(0,r.kt)("inlineCode",{parentName:"p"},"kpi")," folder in append mode.\nTo run only this transformation in append mode, update the _",(0,r.kt)("em",{parentName:"p"},"config")," file as follows:"),(0,r.kt)("pre",{parentName:"admonition"},(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:"{5-6}","{5-6}":!0},"transform:\n  default:\n    write: OVERWRITE\n  tasks:\n    - name: customers_kpi\n      write: APPEND\n"))),(0,r.kt)("p",null,"That's it! We can now run the transform as follows:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-sh"},"$ cd $HOME/myproject\n$ $HOME/starlake/starlake.sh transform --name kpi.customers_kpi\n")),(0,r.kt)("p",null,"The transform job will run against the connectionRef defined in the application.sl.yml file."),(0,r.kt)("h2",{id:"jinja2-transforms"},"Jinja2 Transforms"),(0,r.kt)("p",null,"Starlake Transforms support Jinja2 templating inside SQL requests.\nTo illustrate how this works, we will create two tables, one containing customers living in France\nand another one containing customers living in the USA."),(0,r.kt)("p",null,"We create a file ",(0,r.kt)("inlineCode",{parentName:"p"},"metadata/transform/kpi/bycountry.sql")," with the following content"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-SQL"},"select * from sales.customers where lower(country) like lower('{{ p_country }}')\n")),(0,r.kt)("p",null,"By creating the sql file in the kpi folder, we instruct Starlake to put the result of the query in a schema (dataset in BigQuery)\nnamed 'kpi'.\nBut we need the french customers to be stored in the table ",(0,r.kt)("inlineCode",{parentName:"p"},"cust_france")," and the american customers in the table ",(0,r.kt)("inlineCode",{parentName:"p"},"cust_usa"),"."),(0,r.kt)("p",null,"Since by default the table is named after the SQL file name, ",(0,r.kt)("inlineCode",{parentName:"p"},"bycountry.sql")," will create a table named ",(0,r.kt)("inlineCode",{parentName:"p"},"bycountry"),".\nWe need to configure a dynamic table name based on the country name in the YAML configuration file.\nTherefore, we need to update the YAML file ",(0,r.kt)("inlineCode",{parentName:"p"},"metadata/transform/kpi/_config.sl.yml"),".\nThis file will instruct where the result of the SQL request will be stored."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:"{5-7}","{5-7}":!0},"transform:\n  tasks:\n    - name: customers_kpi\n      write: APPEND\n    - name: bycountry # We define the name of the transform.\n      table: cust_{{ p_country }} # We define the table name based on the country name.\n      write: OVERWRITE # We overwrite the table each time the job is executed.\n\n")),(0,r.kt)(o.Z,{groupId:"customers",mdxType:"Tabs"},(0,r.kt)(l.Z,{value:"france",label:"French customers table",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-sh"},"$ cd $HOME/myproject\n$ $HOME/starlake/starlake.sh transform --name kpi.bycountry --options p_country=France\n"))),(0,r.kt)(l.Z,{value:"usa",label:"American customers table",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-sh"},"$ cd $HOME/myproject\n$ $HOME/starlake/starlake.sh transform --name kpi.bycountry --options p_country=USA\n")))),(0,r.kt)("p",null,"The ",(0,r.kt)("inlineCode",{parentName:"p"},"--options")," allow to define variables that will be used for substitution and thus allowing a query to be parameterized."),(0,r.kt)("admonition",{type:"note"},(0,r.kt)("p",{parentName:"admonition"},"The ",(0,r.kt)("inlineCode",{parentName:"p"},"--options")," parameter is optional. If not provided,\nthe transform will be executed with the default values defined in the env.{{SL_ENV}}.sl.yml  file.")),(0,r.kt)("h2",{id:"pythonpyspark-transforms"},"Python/PySpark Transforms"),(0,r.kt)("p",null,"You mays also use Python/PySpark to create your transforms. The output of the job must be a Spark DataFrame registered as a temporary view named ",(0,r.kt)("inlineCode",{parentName:"p"},"SL_THIS"),".\nThe dataframe will be saved in a table named after the filename.\nBelow is an example:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-python",metastring:'title="metadata/transform/kpi/pi.py"',title:'"metadata/transform/kpi/pi.py"'},'import sys\nfrom random import random\nfrom operator import add\n\nfrom pyspark.sql import SparkSession\n\n# From Apache Spark example\nif __name__ == "__main__":\n    # get Spark context\n    spark = SparkSession.builder.getOrCreate()\n\n    # run any python/pyspark transform below\n    partitions = 2\n    n = 100000 * partitions\n\n    def f(_: int) -> float:\n        x = random() * 2 - 1\n        y = random() * 2 - 1\n        return 1 if x ** 2 + y ** 2 <= 1 else 0\n    count = spark.sparkContext.parallelize(range(1, n + 1), partitions).map(f).reduce(add)\n    result = 4.0 * count / n\n\n    # put the result in a spark dataframe\n    df = spark.createDataFrame([[result]])\n    # register the dataframe as a temporary view named SL_THIS\n    df.registerTempTable("SL_THIS")\n')),(0,r.kt)("h2",{id:"targeting-another-datawarehouse"},"Targeting another datawarehouse"),(0,r.kt)(o.Z,{groupId:"warehouses",mdxType:"Tabs"},(0,r.kt)(l.Z,{value:"bq",label:"BigQuery",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="metadata/transform/kpi/_config.sl.yml"',title:'"metadata/transform/kpi/_config.sl.yml"'},"transform:\n  tasks:\n    - table: cust_{{ p_country }}\n      connectionRef: bigquery\n"))),(0,r.kt)(l.Z,{value:"databricks",label:"Databricks",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="metadata/transform/kpi/_config.sl.yml"',title:'"metadata/transform/kpi/_config.sl.yml"'},"transform:\n  tasks:\n    - table: cust_{{ p_country }}\n      connectionRef: databricks\n"))),(0,r.kt)(l.Z,{value:"hive",label:"Hive",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="metadata/transform/kpi/_config.sl.yml"',title:'"metadata/transform/kpi/_config.sl.yml"'},"transform:\n  tasks:\n    - table: cust_{{ p_country }}\n      connectionRef: spark\n"))),(0,r.kt)(l.Z,{value:"redshift",label:"Redshift",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="metadata/transform/kpi/_config.sl.yml"',title:'"metadata/transform/kpi/_config.sl.yml"'},"transform:\n  tasks:\n    - table: cust_{{ p_country }}\n      connectionRef: redshift\n")),(0,r.kt)("p",null,"Amazon Redshift uses a JDBC URL and a specific format. We need to define our redshift connection in the metadata/application.sl.yml file as follows:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="metadata/application.sl.yml"',title:'"metadata/application.sl.yml"'},'connections:\n  redshift:\n    sparkFormat: "com.databricks.spark.redshift"\n    options:\n      url: "jdbc:redshift://redshifthost:5439/database",\n      user: "username",\n      password: "pass",\n      tempdir: "s3n://path/for/temp/data",\n      aws_iam_role: "arn:aws:iam::123456789000:role/redshift_iam_role"\n'))),(0,r.kt)(l.Z,{value:"snowflake",label:"Snowflake",mdxType:"TabItem"},(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="metadata/transform/kpi/_config.sl.yml"',title:'"metadata/transform/kpi/_config.sl.yml"'},"transform:\n  tasks:\n    - table: cust_{{ p_country }}\n      connectionRef: snowflake\n")),(0,r.kt)("p",null,"Snowflake uses a JDBC URL and a specific format. We need to define our snowflake connection in the metadata/application.sl.yml file as follows:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-yaml",metastring:'title="metadata/application.sl.yml"',title:'"metadata/application.sl.yml"'},'connections:\n  snowflake:\n    sparkFormat = "net.snowflake.spark.snowflake"\n    options:\n      url: "jdbc:snowflake://myorganization-myaccount.snowflakecomputing.com/",\n      user: "username",\n      password: "pass",\n      account: "myorganization-myaccount",\n      warehouse: "mywh",\n      autopushdown: "off" # to pushdown set to \'on\'\n      db: "mydb",\n      schema: "public"\n\n')))))}f.isMDXComponent=!0}}]);