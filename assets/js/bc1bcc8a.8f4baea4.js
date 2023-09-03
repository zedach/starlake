"use strict";(self.webpackChunkstarlake_docs=self.webpackChunkstarlake_docs||[]).push([[5781],{3905:(e,t,n)=>{n.d(t,{Zo:()=>u,kt:()=>h});var a=n(7294);function i(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function r(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){i(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,a,i=function(e,t){if(null==e)return{};var n,a,i={},o=Object.keys(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||(i[n]=e[n]);return i}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(i[n]=e[n])}return i}var s=a.createContext({}),c=function(e){var t=a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):r(r({},t),e)),n},u=function(e){var t=c(e.components);return a.createElement(s.Provider,{value:t},e.children)},p="mdxType",m={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},d=a.forwardRef((function(e,t){var n=e.components,i=e.mdxType,o=e.originalType,s=e.parentName,u=l(e,["components","mdxType","originalType","parentName"]),p=c(n),d=i,h=p["".concat(s,".").concat(d)]||p[d]||m[d]||o;return n?a.createElement(h,r(r({ref:t},u),{},{components:n})):a.createElement(h,r({ref:t},u))}));function h(e,t){var n=arguments,i=t&&t.mdxType;if("string"==typeof e||i){var o=n.length,r=new Array(o);r[0]=d;var l={};for(var s in t)hasOwnProperty.call(t,s)&&(l[s]=t[s]);l.originalType=e,l[p]="string"==typeof e?e:i,r[1]=l;for(var c=2;c<o;c++)r[c]=n[c];return a.createElement.apply(null,r)}return a.createElement.apply(null,n)}d.displayName="MDXCreateElement"},4414:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>s,contentTitle:()=>r,default:()=>m,frontMatter:()=>o,metadata:()=>l,toc:()=>c});var a=n(7462),i=(n(7294),n(3905));const o={sidebar_position:1},r="How to contribute",l={unversionedId:"devguide/contribute",id:"version-0.8.0/devguide/contribute",title:"How to contribute",description:"This guide documents the best way to make various types of contribution to Starlake Data Pipeline, including what is required before submitting a code change.",source:"@site/versioned_docs/version-0.8.0/devguide/contribute.md",sourceDirName:"devguide",slug:"/devguide/contribute",permalink:"/starlake/docs/devguide/contribute",draft:!1,editUrl:"https://github.com/starlake-ai/starlake/edit/master/docs/versioned_docs/version-0.8.0/devguide/contribute.md",tags:[],version:"0.8.0",sidebarPosition:1,frontMatter:{sidebar_position:1},sidebar:"starlakeSidebar",previous:{title:"yml2xls",permalink:"/starlake/docs/cli/yml2xls"}},s={},c=[{value:"How to contribute to this documentation",id:"how-to-contribute-to-this-documentation",level:2},{value:"Contributing Issue Reports",id:"contributing-issue-reports",level:3},{value:"Contributing to architecture documentation",id:"contributing-to-architecture-documentation",level:3},{value:"Contributing by Reviewing Changes",id:"contributing-by-reviewing-changes",level:3},{value:"The Review Process",id:"the-review-process",level:2},{value:"Contributing Documentation Changes",id:"contributing-documentation-changes",level:2},{value:"Contributing Code Changes",id:"contributing-code-changes",level:2},{value:"Git workflow",id:"git-workflow",level:2},{value:"Merge Request",id:"merge-request",level:2}],u={toc:c},p="wrapper";function m(e){let{components:t,...n}=e;return(0,i.kt)(p,(0,a.Z)({},u,n,{components:t,mdxType:"MDXLayout"}),(0,i.kt)("h1",{id:"how-to-contribute"},"How to contribute"),(0,i.kt)("p",null,"This guide documents the best way to make various types of contribution to Starlake Data Pipeline, including what is required before submitting a code change."),(0,i.kt)("p",null,"Contributing to Starlake Data Pipeline doesn\u2019t just mean writing code. Testing the application and improving documentation are also welcome."),(0,i.kt)("h2",{id:"how-to-contribute-to-this-documentation"},"How to contribute to this documentation"),(0,i.kt)("p",null,"This documentation is rendered by ",(0,i.kt)("a",{parentName:"p",href:"https://docusaurus.io/"},"Docusaurus")," and has to be written in Markdown markup language."),(0,i.kt)("p",null,"In order to render this documentation on your side, please follow the instructions available ",(0,i.kt)("a",{parentName:"p",href:"https://github.com/starlake-ai/starlake/blob/master/docs/README.md"},"here")),(0,i.kt)("h3",{id:"contributing-issue-reports"},"Contributing Issue Reports"),(0,i.kt)("p",null,"Issue can be of many kinds:"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"new feature request"),(0,i.kt)("li",{parentName:"ul"},"bug report"),(0,i.kt)("li",{parentName:"ul"},"documentation")),(0,i.kt)("p",null,"Bug reports are only useful however if they include enough information to understand, isolate and ideally reproduce the bug. Simply encountering an error does not mean a bug should be reported. Search issues before creating it. Unreproducible bugs, or simple error reports, may be closed."),(0,i.kt)("p",null,"They have to be reported by creating an issue based on and following the appropriate template."),(0,i.kt)("p",null,"The issue life cycle is:"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"an issue is created, following the appropriate template"),(0,i.kt)("li",{parentName:"ul"},"if any clarification is needed, a technical exchange will follow in the issue comments"),(0,i.kt)("li",{parentName:"ul"},'when the issue is considered to be clear enough by at least 2 animators of the project, the issue is flagged as "ready"')),(0,i.kt)("h3",{id:"contributing-to-architecture-documentation"},"Contributing to architecture documentation"),(0,i.kt)("p",null,"Architecture documents will be exposed by read the docs, in ",(0,i.kt)("inlineCode",{parentName:"p"},".rst")," format, in the ",(0,i.kt)("inlineCode",{parentName:"p"},"doc/architecture")," folder.\nAny architecture changes, update or proposal can be made through a documentation issue, followed by a ",(0,i.kt)("inlineCode",{parentName:"p"},"doc/")," pull request (c.f. below)."),(0,i.kt)("h3",{id:"contributing-by-reviewing-changes"},"Contributing by Reviewing Changes"),(0,i.kt)("p",null,"Changes to Starlake Data Pipeline source code are proposed, reviewed and committed via Github merge requests (described later). Anyone can view and comment on active changes here. Reviewing others\u2019 changes is a good way to learn how the change process works and gain exposure to activity in various parts of the code. You can help by reviewing the changes and asking questions or pointing out issues \u2013 as simple as typos or small issues of style."),(0,i.kt)("h2",{id:"the-review-process"},"The Review Process"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"Other reviewers, including committers, may comment on the changes and suggest modifications. Changes can be added by simply pushing more commits to the same branch."),(0,i.kt)("li",{parentName:"ul"},"Lively, polite, rapid technical debate is encouraged from everyone. The outcome may be a rejection of the entire change."),(0,i.kt)("li",{parentName:"ul"},"Reviewers can indicate that a change looks suitable for merging with a comment."),(0,i.kt)("li",{parentName:"ul"},"Sometimes, other changes will be merged which conflict with your pull request\u2019s changes. The merge request can\u2019t be merged until the conflict is resolved."),(0,i.kt)("li",{parentName:"ul"},"Try to be responsive to the discussion rather than let days pass between replies.")),(0,i.kt)("h2",{id:"contributing-documentation-changes"},"Contributing Documentation Changes"),(0,i.kt)("p",null,"To propose a change to documentation, you have to create an issue first, then edit the Sphinx source files in Starlake Data Pipeline\u2019s docs/ directory and try to build it by following the process described in ",(0,i.kt)("inlineCode",{parentName:"p"},"building.rst"),". The process to propose a doc change is otherwise the same as the process for proposing code changes below."),(0,i.kt)("h2",{id:"contributing-code-changes"},"Contributing Code Changes"),(0,i.kt)("p",null,"Code changes are all related to an issue. If you plan to contribute code changes, please read carrefully the following sections."),(0,i.kt)("h2",{id:"git-workflow"},"Git workflow"),(0,i.kt)("p",null,"Starlake Data Pipeline source code has the following branches' type:"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"master")," is where all issues are merged to and it is the only way to commit to master"),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"[X.Y].x")," is a release branch branched from ",(0,i.kt)("inlineCode",{parentName:"li"},"master")," Each commits on it are related to a fix branch."),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"dev/CDP-[NUM]")," is a development branch branched from ",(0,i.kt)("inlineCode",{parentName:"li"},"master")," related to an issue with the id ","[NUM]"," with an improvement purpose."),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"fix/CDP-[NUM]")," is a development branch branched from ",(0,i.kt)("inlineCode",{parentName:"li"},"[X.Y].x")," related to an issue with the id ","[NUM]"," and where ",(0,i.kt)("inlineCode",{parentName:"li"},"[X.Y].x")," is an ",(0,i.kt)("strong",{parentName:"li"},"active branch")," and the ",(0,i.kt)("strong",{parentName:"li"},"most recent release")," affected by the issue. Fix branches are merged to the affected release and to master. If needed, that commit can be cherry picked to older releases."),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"doc/CDP-[NUM]")," is a documentation branch branched from ",(0,i.kt)("inlineCode",{parentName:"li"},"master")," or ",(0,i.kt)("inlineCode",{parentName:"li"},"[X.Y].x")," related to an issue with the id ","[NUM]",". If it's a ",(0,i.kt)("strong",{parentName:"li"},"release branch"),", it has to be the ",(0,i.kt)("strong",{parentName:"li"},"most recent release possible"),".")),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-text"},"\n              o--o--    fix/CDP-101\n             /\n         o--o--o--      1.0.0\n        /       \\\n    o--o--o--o---o--o-- master\n     \\        \\\n      \\        o--o--   doc/CDP-99\n       o--o--           dev/CDP-100\n")),(0,i.kt)("h2",{id:"merge-request"},"Merge Request"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"Create a new branch following the pattern listed above"),(0,i.kt)("li",{parentName:"ul"},"Commit your changes. The commit message has to match the following pattern:")),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-text"},"        <type>[optional scope]: <description>\n\n        [optional body]\n\n        [optional footer]\n")),(0,i.kt)("p",null,"The commit contains the following structural elements, to communicate intent to the consumers of your library:"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},(0,i.kt)("strong",{parentName:"p"},"fix"),": a commit of the type ",(0,i.kt)("inlineCode",{parentName:"p"},"fix")," patches a bug in your codebase (this correlates with PATCH in semantic versioning).")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},(0,i.kt)("strong",{parentName:"p"},"feat"),": a commit of the type ",(0,i.kt)("inlineCode",{parentName:"p"},"feat")," introduces a new feature to the codebase (this correlates with MINOR in semantic versioning).")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},(0,i.kt)("strong",{parentName:"p"},"BREAKING CHANGE"),": a commit that has the text ",(0,i.kt)("inlineCode",{parentName:"p"},"BREAKING CHANGE:")," at the beginning of its optional body or footer section introduces a breaking API change (correlating with MAJOR in semantic versioning). A breaking change can be part of commits of any type. e.g., a fix:, feat: & chore: types would all be valid, in addition to any other type.")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Others: commit types other than ",(0,i.kt)("inlineCode",{parentName:"p"},"fix:")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"feat:")," are allowed, for example ",(0,i.kt)("inlineCode",{parentName:"p"},"commitlint-config-conventional <https://github.com/marionebl/commitlint/tree/master/%40commitlint/config-conventional>"),"_ recommends ",(0,i.kt)("inlineCode",{parentName:"p"},"chore:"),", ",(0,i.kt)("inlineCode",{parentName:"p"},"docs:"),", ",(0,i.kt)("inlineCode",{parentName:"p"},"style:"),", ",(0,i.kt)("inlineCode",{parentName:"p"},"refactor:"),", ",(0,i.kt)("inlineCode",{parentName:"p"},"perf:"),", ",(0,i.kt)("inlineCode",{parentName:"p"},"test:"),", and others. We also recommend improvement for commits that improve a current implementation without adding a new feature or fixing a bug. Notice these types are not mandated by the conventional commits specification, and have no implicit effect in semantic versioning (unless they include a BREAKING CHANGE, which is NOT recommended).")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"A scope may be provided to a commit\u2019s type, to provide additional contextual information and is contained within parenthesis, e.g., feat(parser): add ability to parse arrays.        ")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Run ",(0,i.kt)("inlineCode",{parentName:"p"},"test")," SBT task")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Run ",(0,i.kt)("inlineCode",{parentName:"p"},"scalafmt")," SBT task")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Push commits to your branch")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Open a merge request against the branch you branched from."),(0,i.kt)("ul",{parentName:"li"},(0,i.kt)("li",{parentName:"ul"},"The merge request's title has to match the following pattern: ",(0,i.kt)("inlineCode",{parentName:"li"},"[CDP-[NUM]]: [My message]")," where ","[NUM]"," is the related issue number. E.g: ",(0,i.kt)("inlineCode",{parentName:"li"},"[CDP-42]: My great contribution"),"."),(0,i.kt)("li",{parentName:"ul"},"The pull request's body has to include: \"closes #","[NUM]",'" where ',"[NUM]"," is the related issue number. "),(0,i.kt)("li",{parentName:"ul"},"If the merge request is in progress, please add ",(0,i.kt)("inlineCode",{parentName:"li"},"WIP:")," in front of the title."))),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Jenkins automatic merge request builder will test your changes")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Jenkins will update the pipeline status of the merge request.")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Watch for the results, and investigate and fix failures promptly."),(0,i.kt)("ul",{parentName:"li"},(0,i.kt)("li",{parentName:"ul"},"Fixes can simply be pushed to the same branch from which you opened your merge request"),(0,i.kt)("li",{parentName:"ul"},"Jenkins will automatically re-test when new commits are pushed"),(0,i.kt)("li",{parentName:"ul"},"If the tests failed for reasons unrelated to the change (e.g. Jenkins outage), then a committer can request a re-test with \u201cretest\u201d.")))),(0,i.kt)("h1",{id:"code-style-guide"},"Code Style Guide"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"Starlake Data Pipeline uses ",(0,i.kt)("inlineCode",{parentName:"li"},"scalafmt")," to format scala codes. Configuration is located in the file ",(0,i.kt)("inlineCode",{parentName:"li"},".scalafmt")," at the root of the project. Use ",(0,i.kt)("inlineCode",{parentName:"li"},"scalafmt")," SBT task to format your code and use ",(0,i.kt)("inlineCode",{parentName:"li"},"scalafmtCheck")," SBT task to validate your code."),(0,i.kt)("li",{parentName:"ul"},"Git commit log is linted with ",(0,i.kt)("inlineCode",{parentName:"li"},"gitlint"),". Configuration is located in the file ",(0,i.kt)("inlineCode",{parentName:"li"},".gitlint"),".",(0,i.kt)("ul",{parentName:"li"},(0,i.kt)("li",{parentName:"ul"},"Use ",(0,i.kt)("inlineCode",{parentName:"li"},"gitlint --commits master..HEAD")," to validate your commit log."),(0,i.kt)("li",{parentName:"ul"},"You can install commit-msg hook with ",(0,i.kt)("inlineCode",{parentName:"li"},"gitlint install-hook"),". It will prevent you from committing with a wrong message.")))),(0,i.kt)("h1",{id:"build-settings"},"Build settings"),(0,i.kt)("p",null,"The CI currently compiles with java 11, so we suggest you to use this version if you plan to contribute."),(0,i.kt)("p",null,"If you plan to build from powershell, you may want to set the input and output settings of the terminal as utf-8.\nThis can be done as follows: ",(0,i.kt)("inlineCode",{parentName:"p"},"$OutputEncoding = [console]::InputEncoding = [console]::OutputEncoding = New-Object System.Text.UTF8Encoding")))}m.isMDXComponent=!0}}]);