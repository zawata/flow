"use strict";(self.webpackChunknew_website=self.webpackChunknew_website||[]).push([[5169],{18430:(e,n,t)=>{t.d(n,{V:()=>p,v:()=>o});var a=t(96540);function p(e){let{version:n}=e;return a.createElement("span",{class:"version added",title:"Added in "+n},"\u2265",n)}function o(e){let{version:n}=e;return a.createElement("span",{class:"version removed",title:"Removed after "+n},"\u2264",n)}},5169:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>s,contentTitle:()=>r,default:()=>u,frontMatter:()=>i,metadata:()=>d,toc:()=>l});var a=t(58168),p=(t(96540),t(15680)),o=(t(60681),t(18430));const i={title:"Mapped Types",slug:"/types/mapped-types"},r=void 0,d={unversionedId:"types/mapped-types",id:"types/mapped-types",title:"Mapped Types",description:"Flow's mapped types allow you to transform object types. They are useful for modeling complex runtime operations over objects.",source:"@site/docs/types/mapped-types.md",sourceDirName:"types",slug:"/types/mapped-types",permalink:"/en/docs/types/mapped-types",draft:!1,editUrl:"https://github.com/facebook/flow/edit/main/website/docs/types/mapped-types.md",tags:[],version:"current",frontMatter:{title:"Mapped Types",slug:"/types/mapped-types"},sidebar:"docsSidebar",previous:{title:"Conditional Types",permalink:"/en/docs/types/conditional"},next:{title:"Type Guards",permalink:"/en/docs/types/type-guards"}},s={},l=[{value:"Basic Usage",id:"toc-basic-usage",level:2},{value:"Mapped Type Sources",id:"toc-mapped-type-sources",level:2},{value:"Distributive Mapped Types",id:"toc-distributive-mapped-types",level:2},{value:"Property Modifiers",id:"toc-property-modifiers",level:2},{value:'Mapped Type on Arrays  <SinceVersion version="0.246" />',id:"toc-mapped-type-on-arrays",level:2},{value:"Adoption",id:"toc-adoption",level:2}],m={toc:l};function u(e){let{components:n,...t}=e;return(0,p.mdx)("wrapper",(0,a.A)({},m,t,{components:n,mdxType:"MDXLayout"}),(0,p.mdx)("p",null,"Flow's mapped types allow you to transform object types. They are useful for modeling complex runtime operations over objects."),(0,p.mdx)("h2",{id:"toc-basic-usage"},"Basic Usage"),(0,p.mdx)("p",null,"Mapped Types have syntax similar to indexed object types but use the ",(0,p.mdx)("inlineCode",{parentName:"p"},"in")," keyword:"),(0,p.mdx)("pre",null,(0,p.mdx)("code",{parentName:"pre",className:"language-flow",metastring:"[]","[]":!0},"type O = {foo: number, bar: string};\n\ntype Methodify<T> = () => T;\n\ntype MappedType = {[key in keyof O]: Methodify<O[key]>};\n")),(0,p.mdx)("p",null,"In this example, ",(0,p.mdx)("inlineCode",{parentName:"p"},"MappedType")," has all of the keys from ",(0,p.mdx)("inlineCode",{parentName:"p"},"O")," with all of the value types transformed by\n",(0,p.mdx)("inlineCode",{parentName:"p"},"Methoditfy<O[key]>"),". The ",(0,p.mdx)("inlineCode",{parentName:"p"},"key")," variable is substituted for each key in ",(0,p.mdx)("inlineCode",{parentName:"p"},"O")," when creating the property, so\nthis type evaluates to:"),(0,p.mdx)("pre",null,(0,p.mdx)("code",{parentName:"pre"},"{\n  foo: Methodify<O['foo']>,\n  bar: Methodify<O['bar']>,\n}\n= {\n  foo: () => number,\n  bar: () => string,\n}\n")),(0,p.mdx)("h2",{id:"toc-mapped-type-sources"},"Mapped Type Sources"),(0,p.mdx)("p",null,"We call the type that comes after the ",(0,p.mdx)("inlineCode",{parentName:"p"},"in")," keyword the ",(0,p.mdx)("em",{parentName:"p"},"source")," of the mapped type. The source of\na mapped type must be a subtype of ",(0,p.mdx)("inlineCode",{parentName:"p"},"string | number | symbol"),":"),(0,p.mdx)("pre",null,(0,p.mdx)("code",{parentName:"pre",className:"language-flow",metastring:'[{"startLine":1,"startColumn":28,"endLine":1,"endColumn":34,"description":"Cannot instantiate mapped type [1] because boolean [2] is incompatible with `string | number | symbol`, so it cannot be used to generate keys for mapped type [1]. [incompatible-type]"}]','[{"startLine":1,"startColumn":28,"endLine":1,"endColumn":34,"description":"Cannot':!0,instantiate:!0,mapped:!0,type:!0,"[1]":!0,because:!0,boolean:!0,"[2]":!0,is:!0,incompatible:!0,with:!0,"`string":!0,"|":!0,number:!0,"symbol`,":!0,so:!0,it:!0,cannot:!0,be:!0,used:!0,to:!0,generate:!0,keys:!0,for:!0,"[1].":!0,'[incompatible-type]"}]':!0},"type MappedType = {[key in boolean]: number}; // ERROR!\n")),(0,p.mdx)("p",null,"Typically, you'll want to create a mapped type based on another object type. In this case, you\nshould write your mapped type using an inline ",(0,p.mdx)("inlineCode",{parentName:"p"},"keyof"),":"),(0,p.mdx)("pre",null,(0,p.mdx)("code",{parentName:"pre",className:"language-flow",metastring:"[]","[]":!0},"type GetterOf<T> = () => T;\ntype Obj = {foo: number, bar: string};\ntype MappedObj = {[key in keyof Obj]: GetterOf<Obj[key]>};\n")),(0,p.mdx)("blockquote",null,(0,p.mdx)("p",{parentName:"blockquote"},"NOTE: ",(0,p.mdx)("inlineCode",{parentName:"p"},"keyof")," only works inline in mapped types for now. Full support for ",(0,p.mdx)("inlineCode",{parentName:"p"},"keyof")," is not yet available.")),(0,p.mdx)("p",null,"But you do not need to use an object to generate a mapped type. You can also use a union of string\nliteral types to represent the keys of an object type:"),(0,p.mdx)("pre",null,(0,p.mdx)("code",{parentName:"pre",className:"language-flow",metastring:"[]","[]":!0},"type Union = 'foo' | 'bar' | 'baz';\ntype MappedType = {[key in Union]: number};\n// = {foo: number, bar: number, baz: number};\n")),(0,p.mdx)("p",null,"Importantly, when using string literals the union is collapsed into a ",(0,p.mdx)("em",{parentName:"p"},"single object type"),":"),(0,p.mdx)("pre",null,(0,p.mdx)("code",{parentName:"pre",className:"language-flow",metastring:"[]","[]":!0},"type MappedTypeFromKeys<Keys: string> = {[key in Keys]: number};\ntype MappedFooAndBar = MappedTypeFromKeys<'foo' | 'bar'>;\n// = {foo: number, bar: number}, not {foo: number} | {bar: number}\n")),(0,p.mdx)("p",null,"If you use a type like ",(0,p.mdx)("inlineCode",{parentName:"p"},"number")," or ",(0,p.mdx)("inlineCode",{parentName:"p"},"string")," in the source of your mapped type then Flow will create\nan indexer:"),(0,p.mdx)("pre",null,(0,p.mdx)("code",{parentName:"pre",className:"language-flow",metastring:"[]","[]":!0},"type MappedTypeFromKeys<Keys: string> = {[key in Keys]: number};\ntype MappedFooAndBarWithIndexer = MappedTypeFromKeys<'foo' | 'bar' | string>;\n// = {foo: number, bar: number, [string]: number}\n")),(0,p.mdx)("h2",{id:"toc-distributive-mapped-types"},"Distributive Mapped Types"),(0,p.mdx)("p",null,"When the mapped type uses an inline ",(0,p.mdx)("inlineCode",{parentName:"p"},"keyof")," or a type parameter bound by a ",(0,p.mdx)("inlineCode",{parentName:"p"},"$Keys"),"\nFlow will distribute the mapped type over unions of object types."),(0,p.mdx)("p",null,"For example:"),(0,p.mdx)("pre",null,(0,p.mdx)("code",{parentName:"pre",className:"language-flow",metastring:"[]","[]":!0},"// This mapped type uses keyof inline\ntype MakeAllValuesNumber<O: {...}> = {[key in keyof O]: number};\ntype ObjWithFoo = {foo: string};\ntype ObjWithBar = {bar: string};\n\ntype DistributedMappedType = MakeAllValuesNumber<\n  | ObjWithFoo\n  | ObjWithBar\n>; // = {foo: number} | {bar: number};\n\n// This mapped type uses a type parameter bound by $Keys\ntype Pick<O: {...}, Keys: $Keys<O>> = {[key in Keys]: O[key]};\ntype O1 = {foo: number, bar: number};\ntype O2 = {bar: string, baz: number};\ntype PickBar = Pick<O1 | O2, 'bar'>; // = {bar: number} | {bar: string};\n")),(0,p.mdx)("p",null,"Distributive mapped types will also distribute over ",(0,p.mdx)("inlineCode",{parentName:"p"},"null")," and ",(0,p.mdx)("inlineCode",{parentName:"p"},"undefined"),":"),(0,p.mdx)("pre",null,(0,p.mdx)("code",{parentName:"pre",className:"language-flow",metastring:"[]","[]":!0},"type Distributive<O: ?{...}> = {[key in keyof O]: O[key]};\ntype Obj = {foo: number};\ntype MaybeMapped = Distributive<?Obj>;// = ?{foo: number}\nnull as MaybeMapped; // OK\nundefined as MaybeMapped; // OK\n({foo: 3}) as MaybeMapped; // OK\n")),(0,p.mdx)("h2",{id:"toc-property-modifiers"},"Property Modifiers"),(0,p.mdx)("p",null,"You can also add ",(0,p.mdx)("inlineCode",{parentName:"p"},"+")," or ",(0,p.mdx)("inlineCode",{parentName:"p"},"-")," variance modifiers and the optionality modifier ",(0,p.mdx)("inlineCode",{parentName:"p"},"?")," in mapped types:"),(0,p.mdx)("pre",null,(0,p.mdx)("code",{parentName:"pre",className:"language-flow",metastring:"[]","[]":!0},"type O = {foo: number, bar: string}\ntype ReadOnlyPartialO = {+[key in keyof O]?: O[key]}; // = {+foo?: number, +bar?: string};\n")),(0,p.mdx)("p",null,"When no variance nor optionality modifiers are provided and the mapped type is distributive,\nthe variance and optionality are determined by the input object:"),(0,p.mdx)("pre",null,(0,p.mdx)("code",{parentName:"pre",className:"language-flow",metastring:"[]","[]":!0},"type O = {+foo: number, bar?: string};\ntype Mapped = {[key in keyof O]: O[key]}; // = {+foo: number, bar?: string}\n")),(0,p.mdx)("p",null,"Otherwise, the properties are read-write and required when no property modifiers are present:"),(0,p.mdx)("pre",null,(0,p.mdx)("code",{parentName:"pre",className:"language-flow",metastring:"[]","[]":!0},"type Union = 'foo' | 'bar' | 'baz';\ntype MappedType = {[key in Union]: number};\n// = {foo: number, bar: number, baz: number};\n")),(0,p.mdx)("blockquote",null,(0,p.mdx)("p",{parentName:"blockquote"},"NOTE: Flow does not yet support removing variance or optionality modifiers.")),(0,p.mdx)("h2",{id:"toc-mapped-type-on-arrays"},"Mapped Type on Arrays  ",(0,p.mdx)(o.V,{version:"0.246",mdxType:"SinceVersion"})),(0,p.mdx)("p",null,"Mapped type also works on array or tuple inputs. If the mapped type is in the form of"),(0,p.mdx)("pre",null,(0,p.mdx)("code",{parentName:"pre"},"{[K in keyof <type_1>]: <type_2>}\n")),(0,p.mdx)("p",null,"then ",(0,p.mdx)("inlineCode",{parentName:"p"},"type_1")," is allowed to be an array or tuple type."),(0,p.mdx)("p",null,"This feature will be especially useful if you want to map over elements of a tuple:"),(0,p.mdx)("pre",null,(0,p.mdx)("code",{parentName:"pre",className:"language-flow",metastring:'[{"startLine":5,"startColumn":1,"endLine":5,"endColumn":2,"description":"Cannot cast empty string to indexed access because string [1] is incompatible with boolean [2]. [incompatible-cast]"},{"startLine":6,"startColumn":1,"endLine":6,"endColumn":5,"description":"Cannot cast `false` to indexed access because boolean [1] is incompatible with string [2]. [incompatible-cast]"},{"startLine":8,"startColumn":1,"endLine":8,"endColumn":9,"description":"Cannot assign `true` to `mapped[0]` because tuple element at index `0` [1] labeled `a` is not writable. [cannot-write]"}]','[{"startLine":5,"startColumn":1,"endLine":5,"endColumn":2,"description":"Cannot':!0,cast:!0,empty:!0,string:!0,to:!0,indexed:!0,access:!0,because:!0,"[1]":!0,is:!0,incompatible:!0,with:!0,boolean:!0,"[2].":!0,'[incompatible-cast]"},{"startLine":6,"startColumn":1,"endLine":6,"endColumn":5,"description":"Cannot':!0,"`false`":!0,'[incompatible-cast]"},{"startLine":8,"startColumn":1,"endLine":8,"endColumn":9,"description":"Cannot':!0,assign:!0,"`true`":!0,"`mapped[0]`":!0,tuple:!0,element:!0,at:!0,index:!0,"`0`":!0,labeled:!0,"`a`":!0,not:!0,"writable.":!0,'[cannot-write]"}]':!0},"type Tuple = [+a: number, b?: string];\ntype MappedTuple = {[K in keyof Tuple]: Tuple[K] extends number ? boolean : string};\nconst a: MappedTuple[0] = true;\nconst b: MappedTuple[1] = '';\n'' as MappedTuple[0] // error\nfalse as MappedTuple[1] // error\ndeclare const mapped: MappedTuple;\nmapped[0] = true; // error: cannot-write\n")),(0,p.mdx)("p",null,"For now, the only supported property modifier on array input is the optionality modifier ",(0,p.mdx)("inlineCode",{parentName:"p"},"?"),"."),(0,p.mdx)("pre",null,(0,p.mdx)("code",{parentName:"pre",className:"language-flow",metastring:'[{"startLine":3,"startColumn":21,"endLine":3,"endColumn":49,"description":"Mapped Types do not yet support variance annotations on array inputs. [invalid-mapped-type]"},{"startLine":4,"startColumn":21,"endLine":4,"endColumn":49,"description":"Mapped Types do not yet support variance annotations on array inputs. [invalid-mapped-type]"}]','[{"startLine":3,"startColumn":21,"endLine":3,"endColumn":49,"description":"Mapped':!0,Types:!0,do:!0,not:!0,yet:!0,support:!0,variance:!0,annotations:!0,on:!0,array:!0,"inputs.":!0,'[invalid-mapped-type]"},{"startLine":4,"startColumn":21,"endLine":4,"endColumn":49,"description":"Mapped':!0,'[invalid-mapped-type]"}]':!0},"type Tuple = [+a: number, b?: string];\ntype Supported = {[K in keyof Tuple]?: string};\ntype Unsupported1 = {+[K in keyof Tuple]: string};\ntype Unsupported2 = {-[K in keyof Tuple]: string};\n")),(0,p.mdx)("h2",{id:"toc-adoption"},"Adoption"),(0,p.mdx)("p",null,"To use mapped types, you need to upgrade your infrastructure so that it supports the syntax:"),(0,p.mdx)("ul",null,(0,p.mdx)("li",{parentName:"ul"},(0,p.mdx)("inlineCode",{parentName:"li"},"flow")," and ",(0,p.mdx)("inlineCode",{parentName:"li"},"flow-parser"),": 0.210.0. Between v0.210.0 to v0.211.1, you need to explicitly enable it in your .flowconfig, under the ",(0,p.mdx)("inlineCode",{parentName:"li"},"[options]")," heading, add ",(0,p.mdx)("inlineCode",{parentName:"li"},"mapped_type=true"),"."),(0,p.mdx)("li",{parentName:"ul"},(0,p.mdx)("inlineCode",{parentName:"li"},"prettier"),": 3"),(0,p.mdx)("li",{parentName:"ul"},(0,p.mdx)("inlineCode",{parentName:"li"},"babel")," with ",(0,p.mdx)("inlineCode",{parentName:"li"},"babel-plugin-syntax-hermes-parser"),". See ",(0,p.mdx)("a",{parentName:"li",href:"../../tools/babel/"},"our Babel guide")," for setup instructions."),(0,p.mdx)("li",{parentName:"ul"},(0,p.mdx)("inlineCode",{parentName:"li"},"eslint")," with ",(0,p.mdx)("inlineCode",{parentName:"li"},"hermes-eslint"),". See ",(0,p.mdx)("a",{parentName:"li",href:"../../tools/eslint/"},"our ESLint guide")," for setup instructions.")))}u.isMDXComponent=!0}}]);