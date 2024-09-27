"use strict";(self.webpackChunknew_website=self.webpackChunknew_website||[]).push([[4736],{54736:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>m,contentTitle:()=>l,default:()=>d,frontMatter:()=>i,metadata:()=>r,toc:()=>s});var o=a(58168),n=(a(96540),a(15680));a(60681);const i={title:"Multi-platform Support for React Native",slug:"/react/multiplatform",description:"Flow's support for multiple platforms inside a single React Native codebase"},l=void 0,r={unversionedId:"react/multiplatform",id:"react/multiplatform",title:"Multi-platform Support for React Native",description:"Flow's support for multiple platforms inside a single React Native codebase",source:"@site/docs/react/multiplatform.md",sourceDirName:"react",slug:"/react/multiplatform",permalink:"/en/docs/react/multiplatform",draft:!1,editUrl:"https://github.com/facebook/flow/edit/main/website/docs/react/multiplatform.md",tags:[],version:"current",frontMatter:{title:"Multi-platform Support for React Native",slug:"/react/multiplatform",description:"Flow's support for multiple platforms inside a single React Native codebase"}},m={},s=[{value:"Benefits",id:"toc-benefits",level:2},{value:"Quick Start",id:"toc-quick-start",level:2},{value:"Common Interface Files",id:"toc-common-interface-file",level:2},{value:"Common Interface File in <code>.js.flow</code>",id:"toc-common-interface-file-in-js-flow",level:3},{value:"Common Interface File in <code>.js</code>",id:"toc-common-interface-file-in-js",level:3}],p={toc:s};function d(e){let{components:t,...a}=e;return(0,n.mdx)("wrapper",(0,o.A)({},p,a,{components:t,mdxType:"MDXLayout"}),(0,n.mdx)("admonition",{type:"caution"},(0,n.mdx)("p",{parentName:"admonition"},"The feature is still experimental. Behaviors might change in the future.")),(0,n.mdx)("h2",{id:"toc-benefits"},"Benefits"),(0,n.mdx)("p",null,"React Native supports conditional bundling of files with ",(0,n.mdx)("a",{parentName:"p",href:"https://reactnative.dev/docs/platform-specific-code#platform-specific-extensions"},"platform specific extensions"),". For example, if you have different implementations of an Image component for iOS and Android, you can have an ",(0,n.mdx)("inlineCode",{parentName:"p"},"Image.ios.js")," file and ",(0,n.mdx)("inlineCode",{parentName:"p"},"Image.android.js")," file, and an import of Image can be resolved to either file based on the platform you are targeting."),(0,n.mdx)("p",null,"These platform specific files live under the same repository, but it would normally require two flowconfigs to check them like the following setup:"),(0,n.mdx)("pre",null,(0,n.mdx)("code",{parentName:"pre",className:"language-toml",metastring:"title=.flowconfig",title:".flowconfig"},"; for ios\n[ignore]\n.*\\.android\\.js$\n[options]\nmodule.file_ext=.js\nmodule.file_ext=.ios.js\n")),(0,n.mdx)("pre",null,(0,n.mdx)("code",{parentName:"pre",className:"language-toml",metastring:"title=.flowconfig.android",title:".flowconfig.android"},"; for android\n[ignore]\n; Ignore other platform suffixes\n.*\\.ios\\.js$\n[options]\nmodule.file_ext=.js\nmodule.file_ext=.android.js\n")),(0,n.mdx)("p",null,"Flow's optional React Native multi-platform support allows you to check your entire project with mixed platforms under a single Flow root, so that during the development of a module with both .ios and .android files, you no longer have to run both Flow servers and constantly switch between different servers to see type errors on different platforms."),(0,n.mdx)("h2",{id:"toc-quick-start"},"Quick Start"),(0,n.mdx)("p",null,"You can start by deleting the flowconfig for all other platforms, deleting all the platform specific configs in the only remaining flowconfig, and add the following new lines to the ",(0,n.mdx)("inlineCode",{parentName:"p"},"options")," section:"),(0,n.mdx)("pre",null,(0,n.mdx)("code",{parentName:"pre"},"experimental.multi_platform=true\nexperimental.multi_platform.extensions=.ios\nexperimental.multi_platform.extensions=.android\n")),(0,n.mdx)("p",null,"For example, these are the required changes for the ",(0,n.mdx)("inlineCode",{parentName:"p"},".flowconfig")," example above:"),(0,n.mdx)("pre",null,(0,n.mdx)("code",{parentName:"pre",className:"language-diff",metastring:"title=.flowconfig",title:".flowconfig"},"[ignore]\n- .*\\.android\\.js$\n[options]\nmodule.file_ext=.js\n- module.file_ext=.ios.js\n+ experimental.multi_platform=true\n+ experimental.multi_platform.extensions=.ios\n+ experimental.multi_platform.extensions=.android\n")),(0,n.mdx)("p",null,"After enabling the new configurations, there will likely be new errors. The sections below explain the additional rules that Flow imposes to check a multiplatform React Native project."),(0,n.mdx)("h2",{id:"toc-common-interface-file"},"Common Interface Files"),(0,n.mdx)("p",null,"Suppose you have a file that imports the ",(0,n.mdx)("inlineCode",{parentName:"p"},"Image")," module, but ",(0,n.mdx)("inlineCode",{parentName:"p"},"Image")," module has different iOS and Android implementations as follows:"),(0,n.mdx)("pre",null,(0,n.mdx)("code",{parentName:"pre",className:"language-jsx",metastring:"title=MyReactNativeApp.js",title:"MyReactNativeApp.js"},"import * as React from 'react';\nimport Image from './Image';\n\n<Image src=\"/hello.png\" />;\n<Image src=\"/world.png\" lazyLoading={true} />;\n")),(0,n.mdx)("pre",null,(0,n.mdx)("code",{parentName:"pre",className:"language-jsx",metastring:"title=Image.ios.js",title:"Image.ios.js"},"import * as React from 'react';\n\ntype Props = { src: string, lazyLoading?: boolean };\n\nexport default function Image(props: Props): React.Node { /* ... */ }\n")),(0,n.mdx)("pre",null,(0,n.mdx)("code",{parentName:"pre",className:"language-jsx",metastring:"title=Image.android.js",title:"Image.android.js"},"import * as React from 'react';\n\ntype Props = { src: string, lazyLoading: boolean };\n\nexport default class Image extends React.Components<Props> {\n  static defaultProps: { lazyLoading: boolean } = { lazyLoading: false };\n  render(): React.Node { /* ... */ }\n}\n")),(0,n.mdx)("p",null,"When you enabled multiplatform support, you will likely see that error that the ",(0,n.mdx)("inlineCode",{parentName:"p"},"./Image")," module cannot be resolved. To fix the error, you need to create a common interface file under the same directory:"),(0,n.mdx)("h3",{id:"toc-common-interface-file-in-js-flow"},"Common Interface File in ",(0,n.mdx)("inlineCode",{parentName:"h3"},".js.flow")),(0,n.mdx)("p",null,"One option is to write a common interface file in ",(0,n.mdx)("inlineCode",{parentName:"p"},".js.flow"),":"),(0,n.mdx)("p",null,"With ",(0,n.mdx)("a",{parentName:"p",href:"../component-types"},"Component Types")),(0,n.mdx)("pre",null,(0,n.mdx)("code",{parentName:"pre",className:"language-jsx",metastring:"title=Image.js.flow",title:"Image.js.flow"},"import * as React from 'react';\n\ntype Props = { src: string, lazyLoading?: boolean };\n\ndeclare const Image: component(...Props);\nexport default Image;\n")),(0,n.mdx)("p",null,"With ",(0,n.mdx)("a",{parentName:"p",href:"../types#toc-react-abstractcomponent"},"React.AbstractComponent")),(0,n.mdx)("pre",null,(0,n.mdx)("code",{parentName:"pre",className:"language-jsx",metastring:"title=Image.js.flow",title:"Image.js.flow"},"import * as React from 'react';\n\ntype Props = { src: string, lazyLoading?: boolean };\n\ndeclare const Image: React.AbstractComponent<Props>;\nexport default Image;\n")),(0,n.mdx)("p",null,"Flow will ensure that the module types of both ",(0,n.mdx)("inlineCode",{parentName:"p"},"Image.ios.js")," and ",(0,n.mdx)("inlineCode",{parentName:"p"},"./Image.android.js")," are subtype of the module type of ",(0,n.mdx)("inlineCode",{parentName:"p"},"./Image.js.flow"),". Flow will also ensure that there exists an implementation for each platform you declared in your ",(0,n.mdx)("inlineCode",{parentName:"p"},".flowconfig"),"."),(0,n.mdx)("h3",{id:"toc-common-interface-file-in-js"},"Common Interface File in ",(0,n.mdx)("inlineCode",{parentName:"h3"},".js")),(0,n.mdx)("p",null,"Sometimes you might target desktop platforms in addition to iOS and Android, and you only have a special implementation for one platform, and all the other platforms will use the fallback implementation in a ",(0,n.mdx)("inlineCode",{parentName:"p"},".js")," file. For example:"),(0,n.mdx)("pre",null,(0,n.mdx)("code",{parentName:"pre",className:"language-jsx",metastring:"title=Image.js",title:"Image.js"},"import * as React from 'react';\nimport DefaultImage from 'react-native/Libraries/Image';\n\nexport default DefaultImage;\n")),(0,n.mdx)("pre",null,(0,n.mdx)("code",{parentName:"pre",className:"language-jsx",metastring:"title=Image.ios.js",title:"Image.ios.js"},"import * as React from 'react';\n\ntype Props = { src: string, lazyLoading: boolean };\n\nexport default function Image(props: Props): React.Node {\n  // Custom implementation to take advantage of some unique iOS capabilities\n}\n")),(0,n.mdx)("p",null,"In this case, Flow will use the ",(0,n.mdx)("inlineCode",{parentName:"p"},".js")," file as the common interface file, and check all other platform-specific implementation files' against the ",(0,n.mdx)("inlineCode",{parentName:"p"},".js")," file. Since the ",(0,n.mdx)("inlineCode",{parentName:"p"},".js")," file is already a fallback implementation, Flow will no longer require that platform-specific implementation files exist for all platforms."))}d.isMDXComponent=!0}}]);