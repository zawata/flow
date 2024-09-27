"use strict";(self.webpackChunknew_website=self.webpackChunknew_website||[]).push([[331],{30331:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>l,contentTitle:()=>s,default:()=>d,frontMatter:()=>r,metadata:()=>i,toc:()=>c});var o=t(58168),a=(t(96540),t(15680));t(60681);const r={title:"Hook Syntax",slug:"/react/hook-syntax"},s=void 0,i={unversionedId:"react/hook-syntax",id:"react/hook-syntax",title:"Hook Syntax",description:"Hook Syntax is first-class syntax and typechecking support for React hooks, bringing hooks into",source:"@site/docs/react/hook-syntax.md",sourceDirName:"react",slug:"/react/hook-syntax",permalink:"/en/docs/react/hook-syntax",draft:!1,editUrl:"https://github.com/facebook/flow/edit/main/website/docs/react/hook-syntax.md",tags:[],version:"current",frontMatter:{title:"Hook Syntax",slug:"/react/hook-syntax"},sidebar:"docsSidebar",previous:{title:"Component Syntax",permalink:"/en/docs/react/component-syntax"},next:{title:"Component Types",permalink:"/en/docs/react/component-types"}},l={},c=[{value:"Basic Usage",id:"basic-usage",level:2},{value:"Hook Type Annotations",id:"hook-type-annotations",level:2},{value:"Enforcing the Rules of React with Hook Syntax",id:"enforcing-the-rules-of-react-with-hook-syntax",level:2},{value:"Preventing Unsafe Mutation",id:"preventing-unsafe-mutation",level:3},{value:"Preventing Conditional Hook Calls",id:"preventing-conditional-hook-calls",level:3},{value:"Preventing Conflation of Hooks and Functions",id:"preventing-conflation-of-hooks-and-functions",level:3}],u={toc:c};function d(e){let{components:n,...t}=e;return(0,a.mdx)("wrapper",(0,o.A)({},u,t,{components:n,mdxType:"MDXLayout"}),(0,a.mdx)("p",null,"Hook Syntax is first-class syntax and typechecking support for React hooks, bringing hooks into\nthe React language as their own entities that are syntactically and semantically distinct from\nregular functions, and using Flow to enforce that the ",(0,a.mdx)("a",{parentName:"p",href:"https://react.dev/reference/rules"},"Rules of React")," aren\u2019t violated."),(0,a.mdx)("h2",{id:"basic-usage"},"Basic Usage"),(0,a.mdx)("p",null,"The primary difference between writing a function and a hook is the ",(0,a.mdx)("inlineCode",{parentName:"p"},"hook")," keyword:"),(0,a.mdx)("pre",null,(0,a.mdx)("code",{parentName:"pre",className:"language-flow",metastring:"[]","[]":!0},"import {useState, useEffect} from 'react';\n\nhook useOnlineStatus(initial: boolean): boolean {\n  const [isOnline, setIsOnline] = useState(initial);\n  useEffect(() => {\n    // ...\n  }, []);\n  return isOnline;\n}\n")),(0,a.mdx)("p",null,"Hooks can be called just like regular functions:"),(0,a.mdx)("pre",null,(0,a.mdx)("code",{parentName:"pre",className:"language-flow",metastring:"[]","[]":!0},"import * as React from 'react';\n\nhook useOnlineStatus(): boolean {\n    return true;\n}\n\ncomponent StatusBar() {\n  const isOnline = useOnlineStatus();\n  return <h1>{isOnline ? '\u2705 Online' : '\u274c Disconnected'}</h1>;\n}\n")),(0,a.mdx)("p",null,"Hooks can be exported just like normal functions:"),(0,a.mdx)("pre",null,(0,a.mdx)("code",{parentName:"pre",className:"language-flow",metastring:"[]","[]":!0},"export hook useNamedExportedHook(): boolean {\n    return true;\n}\n\nexport default hook useDefaultExportedHook(): boolean {\n    return true;\n}\n")),(0,a.mdx)("h2",{id:"hook-type-annotations"},"Hook Type Annotations"),(0,a.mdx)("p",null,"There are a few cases where you might wish to define a value as having the type of a\nhook. Because function types and hook types aren\u2019t compatible (more on this below!),\nwe also introduce a new syntax for hook type annotations, which is simply the\nexisting function type annotation but preceded by hook."),(0,a.mdx)("pre",null,(0,a.mdx)("code",{parentName:"pre",className:"language-js"},"export const useGKOnlineStatus: hook (boolean) => boolean = \n  experiment('show_online_status')\n  ? useOnlineStatus\n  : useAlwaysOnlineStatus\n")),(0,a.mdx)("h2",{id:"enforcing-the-rules-of-react-with-hook-syntax"},"Enforcing the Rules of React with Hook Syntax"),(0,a.mdx)("p",null,"With hook syntax, we can now unambiguously distinguish syntactically between hooks and\nnon-hooks. Flow will use this information to enforce a number of the rules of hooks and\n",(0,a.mdx)("a",{parentName:"p",href:"https://react.dev/reference/rules"},"Rules of React")," generally."),(0,a.mdx)("h3",{id:"preventing-unsafe-mutation"},"Preventing Unsafe Mutation"),(0,a.mdx)("p",null,"According to the ",(0,a.mdx)("a",{parentName:"p",href:"https://react.dev/reference/rules"},"Rules of React"),", refs aren\u2019t allowed\nto be read from or written to while a component is rendering, and the return value of\nother hooks (especially `useState``) cannot be safely mutated directly at all. By making\nFlow aware of hooks as a first-class concept, we can now detect these issues in many cases\nand raise errors early, rather than depending on testing to uncover them."),(0,a.mdx)("pre",null,(0,a.mdx)("code",{parentName:"pre",className:"language-flow",metastring:'[{"startLine":8,"startColumn":9,"endLine":8,"endColumn":11,"description":"Cannot assign `42` to `state.val` because property `val` is not writable. The return value of a React hook [1] cannot be written to. [react-rule-hook-mutation]"},{"startLine":12,"startColumn":8,"endLine":12,"endColumn":10,"description":"Cannot read `current` from `ref` [1] because `ref` values may not be read during render. (https://react.dev/reference/react/useRef). [react-rule-unsafe-ref]"}]','[{"startLine":8,"startColumn":9,"endLine":8,"endColumn":11,"description":"Cannot':!0,assign:!0,"`42`":!0,to:!0,"`state.val`":!0,because:!0,property:!0,"`val`":!0,is:!0,not:!0,"writable.":!0,The:!0,return:!0,value:!0,of:!0,a:!0,React:!0,hook:!0,"[1]":!0,cannot:!0,be:!0,written:!0,"to.":!0,'[react-rule-hook-mutation]"},{"startLine":12,"startColumn":8,"endLine":12,"endColumn":10,"description":"Cannot':!0,read:!0,"`current`":!0,from:!0,"`ref`":!0,values:!0,may:!0,during:!0,"render.":!0,"(https://react.dev/reference/react/useRef).":!0,'[react-rule-unsafe-ref]"}]':!0},"import {useState, useEffect, useRef} from 'react';\nimport * as React from 'react';\n\ncomponent MyComponent() { \n  const ref = useRef<?number>(null);\n  const [state, setState] = useState<{ val: number }>({val: 0});\n\n  state.val = 42; // Flow error: cannot mutate return value of hook\n\n  return (\n    <div>\n      {ref.current /* Flow error: cannot read ref during rendering */}\n    </div>\n  );\n}\n")),(0,a.mdx)("p",null,"Flow currently prevents component props from being modified within the component.\nHook syntax allows us to extend this checking to hooks, and will let us detect and\nraise errors when illegal mutations occur within hook declarations."),(0,a.mdx)("pre",null,(0,a.mdx)("code",{parentName:"pre",className:"language-flow",metastring:'[{"startLine":2,"startColumn":3,"endLine":2,"endColumn":11,"description":"Cannot assign `42` to `values[0]` because read-only arrays cannot be written to. React hook arguments [1] and their nested elements cannot be written to. [react-rule-unsafe-mutation]"}]','[{"startLine":2,"startColumn":3,"endLine":2,"endColumn":11,"description":"Cannot':!0,assign:!0,"`42`":!0,to:!0,"`values[0]`":!0,because:!0,"read-only":!0,arrays:!0,cannot:!0,be:!0,written:!0,"to.":!0,React:!0,hook:!0,arguments:!0,"[1]":!0,and:!0,their:!0,nested:!0,elements:!0,'[react-rule-unsafe-mutation]"}]':!0},"hook useIllegalMutation(values: Array<number>) {\n  values[0] = 42; // Flow error: mutating argument to hook\n  // ...\n}\n")),(0,a.mdx)("h3",{id:"preventing-conditional-hook-calls"},"Preventing Conditional Hook Calls"),(0,a.mdx)("p",null,(0,a.mdx)("a",{parentName:"p",href:"https://react.dev/reference/rules#rules-of-hooks"},"The Rules of Hooks")," prohibit hooks\nfrom being called conditionally. This is covered by ",(0,a.mdx)("a",{parentName:"p",href:"https://www.npmjs.com/package/eslint-plugin-react-hooks"},"React's ESLint plugin"),",\nbut now Flow will check for these violations too."),(0,a.mdx)("pre",null,(0,a.mdx)("code",{parentName:"pre",className:"language-flow",metastring:'[{"startLine":7,"startColumn":26,"endLine":7,"endColumn":42,"description":"Cannot call hook [1] because React hooks cannot be called in conditional contexts. [react-rule-hook]"}]','[{"startLine":7,"startColumn":26,"endLine":7,"endColumn":42,"description":"Cannot':!0,call:!0,hook:!0,"[1]":!0,because:!0,React:!0,hooks:!0,cannot:!0,be:!0,called:!0,in:!0,conditional:!0,"contexts.":!0,'[react-rule-hook]"}]':!0},"hook useOnlineStatus(): boolean {\n    return true;\n}\n\ncomponent StatusBar(shouldShowOnlineStatus: boolean) {\n  if (shouldShowOnlineStatus) {\n    const onlineStatus = useOnlineStatus();\n  }\n\n  return null;\n}\n")),(0,a.mdx)("h3",{id:"preventing-conflation-of-hooks-and-functions"},"Preventing Conflation of Hooks and Functions"),(0,a.mdx)("p",null,"The distinction between hooks and regular functions is reflected in the Flow type system.\nBecause of the different properties that hooks and functions must obey, it\u2019s Flow error\nto pass a value defined as a hook into a position that expects a function type, and\nan error to pass a regular JavaScript function into a position that expects a hook."),(0,a.mdx)("pre",null,(0,a.mdx)("code",{parentName:"pre",className:"language-flow",metastring:'[{"startLine":10,"startColumn":29,"endLine":10,"endColumn":41,"description":"Cannot call `args.map` because function [1] is a React hook but function type [2] is not a hook in the first argument. React component properties [3] and their nested props and elements cannot be written to. React hooks and other functions are not compatible with each other, because hooks cannot be called conditionally. [react-rule-hook-incompatible]"}]','[{"startLine":10,"startColumn":29,"endLine":10,"endColumn":41,"description":"Cannot':!0,call:!0,"`args.map`":!0,because:!0,function:!0,"[1]":!0,is:!0,a:!0,React:!0,hook:!0,but:!0,type:!0,"[2]":!0,not:!0,in:!0,the:!0,first:!0,"argument.":!0,component:!0,properties:!0,"[3]":!0,and:!0,their:!0,nested:!0,props:!0,elements:!0,cannot:!0,be:!0,written:!0,"to.":!0,hooks:!0,other:!0,functions:!0,are:!0,compatible:!0,with:!0,each:!0,"other,":!0,called:!0,"conditionally.":!0,'[react-rule-hook-incompatible]"}]':!0},"import {useState, useEffect} from 'react';\n\nhook useMultiplier(x: number): number {\n  const [y, setY] = useState(1);\n  useEffect(() => { setY(0) })\n  return x * y;\n}\n\ncomponent Mapper(args: Array<number>) {\n  const multArgs = args.map(useMultiplier);\n  \n  return multArgs;\n}\n")),(0,a.mdx)("p",null,"In addition, Flow enforces that callees with hook-like names inside hooks and components\nare indeed hooks. We also ensure that callees inside of regular function definitions\nare never hooks."),(0,a.mdx)("pre",null,(0,a.mdx)("code",{parentName:"pre",className:"language-flow",metastring:'[{"startLine":4,"startColumn":13,"endLine":4,"endColumn":21,"description":"Cannot call hook [1] because React hooks can only be called within components or hooks. [react-rule-hook]"},{"startLine":9,"startColumn":3,"endLine":9,"endColumn":15,"description":"Cannot call hook because callee [1]\'s name does not conform to React hook rules. Hook names must begin with `use` followed by a capitalized letter. [react-rule-hook]"}]','[{"startLine":4,"startColumn":13,"endLine":4,"endColumn":21,"description":"Cannot':!0,call:!0,hook:!0,"[1]":!0,because:!0,React:!0,hooks:!0,can:!0,only:!0,be:!0,called:!0,within:!0,components:!0,or:!0,"hooks.":!0,'[react-rule-hook]"},{"startLine":9,"startColumn":3,"endLine":9,"endColumn":15,"description":"Cannot':!0,callee:!0,"[1]'s":!0,name:!0,does:!0,not:!0,conform:!0,to:!0,"rules.":!0,Hook:!0,names:!0,must:!0,begin:!0,with:!0,"`use`":!0,followed:!0,by:!0,a:!0,capitalized:!0,"letter.":!0,'[react-rule-hook]"}]':!0},"hook useHook() { return null }\n\nfunction regularJavascript() {\n  const x = useHook(); // Flow error: cannot call a hook outside of a component or hook\n}\n\ncomponent Component() { \n  const renamedHook = useHook;\n  renamedHook(); // Flow error: cannot call a hook whose name does not begin with `use`\n\n  return null;\n}\n")))}d.isMDXComponent=!0}}]);