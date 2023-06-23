(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

open Ppxlib
open Ast_builder.Default

(* let ocaml_version = Versions.ocaml_408 *)

module Sedlexing = Flow_sedlexing
module Cset = Sedlex_cset

module UnicodeProperties = struct

  let id_start =
    [0x41, 0x5a; 0x61, 0x7a; 0xaa, 0xaa; 0xb5, 0xb5; 0xba, 0xba;
     0xc0, 0xd6; 0xd8, 0xf6; 0xf8, 0x1ba; 0x1bb, 0x1bb; 0x1bc, 0x1bf;
     0x1c0, 0x1c3; 0x1c4, 0x293; 0x294, 0x294; 0x295, 0x2af; 0x2b0, 0x2c1;
     0x2c6, 0x2d1; 0x2e0, 0x2e4; 0x2ec, 0x2ec; 0x2ee, 0x2ee; 0x370, 0x373;
     0x374, 0x374; 0x376, 0x377; 0x37a, 0x37a; 0x37b, 0x37d; 0x37f, 0x37f;
     0x386, 0x386; 0x388, 0x38a; 0x38c, 0x38c; 0x38e, 0x3a1; 0x3a3, 0x3f5;
     0x3f7, 0x481; 0x48a, 0x52f; 0x531, 0x556; 0x559, 0x559; 0x560, 0x588;
     0x5d0, 0x5ea; 0x5ef, 0x5f2; 0x620, 0x63f; 0x640, 0x640; 0x641, 0x64a;
     0x66e, 0x66f; 0x671, 0x6d3; 0x6d5, 0x6d5; 0x6e5, 0x6e6; 0x6ee, 0x6ef;
     0x6fa, 0x6fc; 0x6ff, 0x6ff; 0x710, 0x710; 0x712, 0x72f; 0x74d, 0x7a5;
     0x7b1, 0x7b1; 0x7ca, 0x7ea; 0x7f4, 0x7f5; 0x7fa, 0x7fa; 0x800, 0x815;
     0x81a, 0x81a; 0x824, 0x824; 0x828, 0x828; 0x840, 0x858; 0x860, 0x86a;
     0x870, 0x887; 0x889, 0x88e; 0x8a0, 0x8c8; 0x8c9, 0x8c9; 0x904, 0x939;
     0x93d, 0x93d; 0x950, 0x950; 0x958, 0x961; 0x971, 0x971; 0x972, 0x980;
     0x985, 0x98c; 0x98f, 0x990; 0x993, 0x9a8; 0x9aa, 0x9b0; 0x9b2, 0x9b2;
     0x9b6, 0x9b9; 0x9bd, 0x9bd; 0x9ce, 0x9ce; 0x9dc, 0x9dd; 0x9df, 0x9e1;
     0x9f0, 0x9f1; 0x9fc, 0x9fc; 0xa05, 0xa0a; 0xa0f, 0xa10; 0xa13, 0xa28;
     0xa2a, 0xa30; 0xa32, 0xa33; 0xa35, 0xa36; 0xa38, 0xa39; 0xa59, 0xa5c;
     0xa5e, 0xa5e; 0xa72, 0xa74; 0xa85, 0xa8d; 0xa8f, 0xa91; 0xa93, 0xaa8;
     0xaaa, 0xab0; 0xab2, 0xab3; 0xab5, 0xab9; 0xabd, 0xabd; 0xad0, 0xad0;
     0xae0, 0xae1; 0xaf9, 0xaf9; 0xb05, 0xb0c; 0xb0f, 0xb10; 0xb13, 0xb28;
     0xb2a, 0xb30; 0xb32, 0xb33; 0xb35, 0xb39; 0xb3d, 0xb3d; 0xb5c, 0xb5d;
     0xb5f, 0xb61; 0xb71, 0xb71; 0xb83, 0xb83; 0xb85, 0xb8a; 0xb8e, 0xb90;
     0xb92, 0xb95; 0xb99, 0xb9a; 0xb9c, 0xb9c; 0xb9e, 0xb9f; 0xba3, 0xba4;
     0xba8, 0xbaa; 0xbae, 0xbb9; 0xbd0, 0xbd0; 0xc05, 0xc0c; 0xc0e, 0xc10;
     0xc12, 0xc28; 0xc2a, 0xc39; 0xc3d, 0xc3d; 0xc58, 0xc5a; 0xc5d, 0xc5d;
     0xc60, 0xc61; 0xc80, 0xc80; 0xc85, 0xc8c; 0xc8e, 0xc90; 0xc92, 0xca8;
     0xcaa, 0xcb3; 0xcb5, 0xcb9; 0xcbd, 0xcbd; 0xcdd, 0xcde; 0xce0, 0xce1;
     0xcf1, 0xcf2; 0xd04, 0xd0c; 0xd0e, 0xd10; 0xd12, 0xd3a; 0xd3d, 0xd3d;
     0xd4e, 0xd4e; 0xd54, 0xd56; 0xd5f, 0xd61; 0xd7a, 0xd7f; 0xd85, 0xd96;
     0xd9a, 0xdb1; 0xdb3, 0xdbb; 0xdbd, 0xdbd; 0xdc0, 0xdc6; 0xe01, 0xe30;
     0xe32, 0xe33; 0xe40, 0xe45; 0xe46, 0xe46; 0xe81, 0xe82; 0xe84, 0xe84;
     0xe86, 0xe8a; 0xe8c, 0xea3; 0xea5, 0xea5; 0xea7, 0xeb0; 0xeb2, 0xeb3;
     0xebd, 0xebd; 0xec0, 0xec4; 0xec6, 0xec6; 0xedc, 0xedf; 0xf00, 0xf00;
     0xf40, 0xf47; 0xf49, 0xf6c; 0xf88, 0xf8c; 0x1000, 0x102a; 0x103f, 0x103f;
     0x1050, 0x1055; 0x105a, 0x105d; 0x1061, 0x1061; 0x1065, 0x1066; 0x106e, 0x1070;
     0x1075, 0x1081; 0x108e, 0x108e; 0x10a0, 0x10c5; 0x10c7, 0x10c7; 0x10cd, 0x10cd;
     0x10d0, 0x10fa; 0x10fc, 0x10fc; 0x10fd, 0x10ff; 0x1100, 0x1248; 0x124a, 0x124d;
     0x1250, 0x1256; 0x1258, 0x1258; 0x125a, 0x125d; 0x1260, 0x1288; 0x128a, 0x128d;
     0x1290, 0x12b0; 0x12b2, 0x12b5; 0x12b8, 0x12be; 0x12c0, 0x12c0; 0x12c2, 0x12c5;
     0x12c8, 0x12d6; 0x12d8, 0x1310; 0x1312, 0x1315; 0x1318, 0x135a; 0x1380, 0x138f;
     0x13a0, 0x13f5; 0x13f8, 0x13fd; 0x1401, 0x166c; 0x166f, 0x167f; 0x1681, 0x169a;
     0x16a0, 0x16ea; 0x16ee, 0x16f0; 0x16f1, 0x16f8; 0x1700, 0x1711; 0x171f, 0x1731;
     0x1740, 0x1751; 0x1760, 0x176c; 0x176e, 0x1770; 0x1780, 0x17b3; 0x17d7, 0x17d7;
     0x17dc, 0x17dc; 0x1820, 0x1842; 0x1843, 0x1843; 0x1844, 0x1878; 0x1880, 0x1884;
     0x1885, 0x1886; 0x1887, 0x18a8; 0x18aa, 0x18aa; 0x18b0, 0x18f5; 0x1900, 0x191e;
     0x1950, 0x196d; 0x1970, 0x1974; 0x1980, 0x19ab; 0x19b0, 0x19c9; 0x1a00, 0x1a16;
     0x1a20, 0x1a54; 0x1aa7, 0x1aa7; 0x1b05, 0x1b33; 0x1b45, 0x1b4c; 0x1b83, 0x1ba0;
     0x1bae, 0x1baf; 0x1bba, 0x1be5; 0x1c00, 0x1c23; 0x1c4d, 0x1c4f; 0x1c5a, 0x1c77;
     0x1c78, 0x1c7d; 0x1c80, 0x1c88; 0x1c90, 0x1cba; 0x1cbd, 0x1cbf; 0x1ce9, 0x1cec;
     0x1cee, 0x1cf3; 0x1cf5, 0x1cf6; 0x1cfa, 0x1cfa; 0x1d00, 0x1d2b; 0x1d2c, 0x1d6a;
     0x1d6b, 0x1d77; 0x1d78, 0x1d78; 0x1d79, 0x1d9a; 0x1d9b, 0x1dbf; 0x1e00, 0x1f15;
     0x1f18, 0x1f1d; 0x1f20, 0x1f45; 0x1f48, 0x1f4d; 0x1f50, 0x1f57; 0x1f59, 0x1f59;
     0x1f5b, 0x1f5b; 0x1f5d, 0x1f5d; 0x1f5f, 0x1f7d; 0x1f80, 0x1fb4; 0x1fb6, 0x1fbc;
     0x1fbe, 0x1fbe; 0x1fc2, 0x1fc4; 0x1fc6, 0x1fcc; 0x1fd0, 0x1fd3; 0x1fd6, 0x1fdb;
     0x1fe0, 0x1fec; 0x1ff2, 0x1ff4; 0x1ff6, 0x1ffc; 0x2071, 0x2071; 0x207f, 0x207f;
     0x2090, 0x209c; 0x2102, 0x2102; 0x2107, 0x2107; 0x210a, 0x2113; 0x2115, 0x2115;
     0x2118, 0x2118; 0x2119, 0x211d; 0x2124, 0x2124; 0x2126, 0x2126; 0x2128, 0x2128;
     0x212a, 0x212d; 0x212e, 0x212e; 0x212f, 0x2134; 0x2135, 0x2138; 0x2139, 0x2139;
     0x213c, 0x213f; 0x2145, 0x2149; 0x214e, 0x214e; 0x2160, 0x2182; 0x2183, 0x2184;
     0x2185, 0x2188; 0x2c00, 0x2c7b; 0x2c7c, 0x2c7d; 0x2c7e, 0x2ce4; 0x2ceb, 0x2cee;
     0x2cf2, 0x2cf3; 0x2d00, 0x2d25; 0x2d27, 0x2d27; 0x2d2d, 0x2d2d; 0x2d30, 0x2d67;
     0x2d6f, 0x2d6f; 0x2d80, 0x2d96; 0x2da0, 0x2da6; 0x2da8, 0x2dae; 0x2db0, 0x2db6;
     0x2db8, 0x2dbe; 0x2dc0, 0x2dc6; 0x2dc8, 0x2dce; 0x2dd0, 0x2dd6; 0x2dd8, 0x2dde;
     0x3005, 0x3005; 0x3006, 0x3006; 0x3007, 0x3007; 0x3021, 0x3029; 0x3031, 0x3035;
     0x3038, 0x303a; 0x303b, 0x303b; 0x303c, 0x303c; 0x3041, 0x3096; 0x309b, 0x309c;
     0x309d, 0x309e; 0x309f, 0x309f; 0x30a1, 0x30fa; 0x30fc, 0x30fe; 0x30ff, 0x30ff;
     0x3105, 0x312f; 0x3131, 0x318e; 0x31a0, 0x31bf; 0x31f0, 0x31ff; 0x3400, 0x4dbf;
     0x4e00, 0xa014; 0xa015, 0xa015; 0xa016, 0xa48c; 0xa4d0, 0xa4f7; 0xa4f8, 0xa4fd;
     0xa500, 0xa60b; 0xa60c, 0xa60c; 0xa610, 0xa61f; 0xa62a, 0xa62b; 0xa640, 0xa66d;
     0xa66e, 0xa66e; 0xa67f, 0xa67f; 0xa680, 0xa69b; 0xa69c, 0xa69d; 0xa6a0, 0xa6e5;
     0xa6e6, 0xa6ef; 0xa717, 0xa71f; 0xa722, 0xa76f; 0xa770, 0xa770; 0xa771, 0xa787;
     0xa788, 0xa788; 0xa78b, 0xa78e; 0xa78f, 0xa78f; 0xa790, 0xa7ca; 0xa7d0, 0xa7d1;
     0xa7d3, 0xa7d3; 0xa7d5, 0xa7d9; 0xa7f2, 0xa7f4; 0xa7f5, 0xa7f6; 0xa7f7, 0xa7f7;
     0xa7f8, 0xa7f9; 0xa7fa, 0xa7fa; 0xa7fb, 0xa801; 0xa803, 0xa805; 0xa807, 0xa80a;
     0xa80c, 0xa822; 0xa840, 0xa873; 0xa882, 0xa8b3; 0xa8f2, 0xa8f7; 0xa8fb, 0xa8fb;
     0xa8fd, 0xa8fe; 0xa90a, 0xa925; 0xa930, 0xa946; 0xa960, 0xa97c; 0xa984, 0xa9b2;
     0xa9cf, 0xa9cf; 0xa9e0, 0xa9e4; 0xa9e6, 0xa9e6; 0xa9e7, 0xa9ef; 0xa9fa, 0xa9fe;
     0xaa00, 0xaa28; 0xaa40, 0xaa42; 0xaa44, 0xaa4b; 0xaa60, 0xaa6f; 0xaa70, 0xaa70;
     0xaa71, 0xaa76; 0xaa7a, 0xaa7a; 0xaa7e, 0xaaaf; 0xaab1, 0xaab1; 0xaab5, 0xaab6;
     0xaab9, 0xaabd; 0xaac0, 0xaac0; 0xaac2, 0xaac2; 0xaadb, 0xaadc; 0xaadd, 0xaadd;
     0xaae0, 0xaaea; 0xaaf2, 0xaaf2; 0xaaf3, 0xaaf4; 0xab01, 0xab06; 0xab09, 0xab0e;
     0xab11, 0xab16; 0xab20, 0xab26; 0xab28, 0xab2e; 0xab30, 0xab5a; 0xab5c, 0xab5f;
     0xab60, 0xab68; 0xab69, 0xab69; 0xab70, 0xabbf; 0xabc0, 0xabe2; 0xac00, 0xd7a3;
     0xd7b0, 0xd7c6; 0xd7cb, 0xd7fb; 0xf900, 0xfa6d; 0xfa70, 0xfad9; 0xfb00, 0xfb06;
     0xfb13, 0xfb17; 0xfb1d, 0xfb1d; 0xfb1f, 0xfb28; 0xfb2a, 0xfb36; 0xfb38, 0xfb3c;
     0xfb3e, 0xfb3e; 0xfb40, 0xfb41; 0xfb43, 0xfb44; 0xfb46, 0xfbb1; 0xfbd3, 0xfd3d;
     0xfd50, 0xfd8f; 0xfd92, 0xfdc7; 0xfdf0, 0xfdfb; 0xfe70, 0xfe74; 0xfe76, 0xfefc;
     0xff21, 0xff3a; 0xff41, 0xff5a; 0xff66, 0xff6f; 0xff70, 0xff70; 0xff71, 0xff9d;
     0xff9e, 0xff9f; 0xffa0, 0xffbe; 0xffc2, 0xffc7; 0xffca, 0xffcf; 0xffd2, 0xffd7;
     0xffda, 0xffdc; 0x10000, 0x1000b; 0x1000d, 0x10026; 0x10028, 0x1003a; 0x1003c, 0x1003d;
     0x1003f, 0x1004d; 0x10050, 0x1005d; 0x10080, 0x100fa; 0x10140, 0x10174; 0x10280, 0x1029c;
     0x102a0, 0x102d0; 0x10300, 0x1031f; 0x1032d, 0x10340; 0x10341, 0x10341; 0x10342, 0x10349;
     0x1034a, 0x1034a; 0x10350, 0x10375; 0x10380, 0x1039d; 0x103a0, 0x103c3; 0x103c8, 0x103cf;
     0x103d1, 0x103d5; 0x10400, 0x1044f; 0x10450, 0x1049d; 0x104b0, 0x104d3; 0x104d8, 0x104fb;
     0x10500, 0x10527; 0x10530, 0x10563; 0x10570, 0x1057a; 0x1057c, 0x1058a; 0x1058c, 0x10592;
     0x10594, 0x10595; 0x10597, 0x105a1; 0x105a3, 0x105b1; 0x105b3, 0x105b9; 0x105bb, 0x105bc;
     0x10600, 0x10736; 0x10740, 0x10755; 0x10760, 0x10767; 0x10780, 0x10785; 0x10787, 0x107b0;
     0x107b2, 0x107ba; 0x10800, 0x10805; 0x10808, 0x10808; 0x1080a, 0x10835; 0x10837, 0x10838;
     0x1083c, 0x1083c; 0x1083f, 0x10855; 0x10860, 0x10876; 0x10880, 0x1089e; 0x108e0, 0x108f2;
     0x108f4, 0x108f5; 0x10900, 0x10915; 0x10920, 0x10939; 0x10980, 0x109b7; 0x109be, 0x109bf;
     0x10a00, 0x10a00; 0x10a10, 0x10a13; 0x10a15, 0x10a17; 0x10a19, 0x10a35; 0x10a60, 0x10a7c;
     0x10a80, 0x10a9c; 0x10ac0, 0x10ac7; 0x10ac9, 0x10ae4; 0x10b00, 0x10b35; 0x10b40, 0x10b55;
     0x10b60, 0x10b72; 0x10b80, 0x10b91; 0x10c00, 0x10c48; 0x10c80, 0x10cb2; 0x10cc0, 0x10cf2;
     0x10d00, 0x10d23; 0x10e80, 0x10ea9; 0x10eb0, 0x10eb1; 0x10f00, 0x10f1c; 0x10f27, 0x10f27;
     0x10f30, 0x10f45; 0x10f70, 0x10f81; 0x10fb0, 0x10fc4; 0x10fe0, 0x10ff6; 0x11003, 0x11037;
     0x11071, 0x11072; 0x11075, 0x11075; 0x11083, 0x110af; 0x110d0, 0x110e8; 0x11103, 0x11126;
     0x11144, 0x11144; 0x11147, 0x11147; 0x11150, 0x11172; 0x11176, 0x11176; 0x11183, 0x111b2;
     0x111c1, 0x111c4; 0x111da, 0x111da; 0x111dc, 0x111dc; 0x11200, 0x11211; 0x11213, 0x1122b;
     0x11280, 0x11286; 0x11288, 0x11288; 0x1128a, 0x1128d; 0x1128f, 0x1129d; 0x1129f, 0x112a8;
     0x112b0, 0x112de; 0x11305, 0x1130c; 0x1130f, 0x11310; 0x11313, 0x11328; 0x1132a, 0x11330;
     0x11332, 0x11333; 0x11335, 0x11339; 0x1133d, 0x1133d; 0x11350, 0x11350; 0x1135d, 0x11361;
     0x11400, 0x11434; 0x11447, 0x1144a; 0x1145f, 0x11461; 0x11480, 0x114af; 0x114c4, 0x114c5;
     0x114c7, 0x114c7; 0x11580, 0x115ae; 0x115d8, 0x115db; 0x11600, 0x1162f; 0x11644, 0x11644;
     0x11680, 0x116aa; 0x116b8, 0x116b8; 0x11700, 0x1171a; 0x11740, 0x11746; 0x11800, 0x1182b;
     0x118a0, 0x118df; 0x118ff, 0x11906; 0x11909, 0x11909; 0x1190c, 0x11913; 0x11915, 0x11916;
     0x11918, 0x1192f; 0x1193f, 0x1193f; 0x11941, 0x11941; 0x119a0, 0x119a7; 0x119aa, 0x119d0;
     0x119e1, 0x119e1; 0x119e3, 0x119e3; 0x11a00, 0x11a00; 0x11a0b, 0x11a32; 0x11a3a, 0x11a3a;
     0x11a50, 0x11a50; 0x11a5c, 0x11a89; 0x11a9d, 0x11a9d; 0x11ab0, 0x11af8; 0x11c00, 0x11c08;
     0x11c0a, 0x11c2e; 0x11c40, 0x11c40; 0x11c72, 0x11c8f; 0x11d00, 0x11d06; 0x11d08, 0x11d09;
     0x11d0b, 0x11d30; 0x11d46, 0x11d46; 0x11d60, 0x11d65; 0x11d67, 0x11d68; 0x11d6a, 0x11d89;
     0x11d98, 0x11d98; 0x11ee0, 0x11ef2; 0x11fb0, 0x11fb0; 0x12000, 0x12399; 0x12400, 0x1246e;
     0x12480, 0x12543; 0x12f90, 0x12ff0; 0x13000, 0x1342e; 0x14400, 0x14646; 0x16800, 0x16a38;
     0x16a40, 0x16a5e; 0x16a70, 0x16abe; 0x16ad0, 0x16aed; 0x16b00, 0x16b2f; 0x16b40, 0x16b43;
     0x16b63, 0x16b77; 0x16b7d, 0x16b8f; 0x16e40, 0x16e7f; 0x16f00, 0x16f4a; 0x16f50, 0x16f50;
     0x16f93, 0x16f9f; 0x16fe0, 0x16fe1; 0x16fe3, 0x16fe3; 0x17000, 0x187f7; 0x18800, 0x18cd5;
     0x18d00, 0x18d08; 0x1aff0, 0x1aff3; 0x1aff5, 0x1affb; 0x1affd, 0x1affe; 0x1b000, 0x1b122;
     0x1b150, 0x1b152; 0x1b164, 0x1b167; 0x1b170, 0x1b2fb; 0x1bc00, 0x1bc6a; 0x1bc70, 0x1bc7c;
     0x1bc80, 0x1bc88; 0x1bc90, 0x1bc99; 0x1d400, 0x1d454; 0x1d456, 0x1d49c; 0x1d49e, 0x1d49f;
     0x1d4a2, 0x1d4a2; 0x1d4a5, 0x1d4a6; 0x1d4a9, 0x1d4ac; 0x1d4ae, 0x1d4b9; 0x1d4bb, 0x1d4bb;
     0x1d4bd, 0x1d4c3; 0x1d4c5, 0x1d505; 0x1d507, 0x1d50a; 0x1d50d, 0x1d514; 0x1d516, 0x1d51c;
     0x1d51e, 0x1d539; 0x1d53b, 0x1d53e; 0x1d540, 0x1d544; 0x1d546, 0x1d546; 0x1d54a, 0x1d550;
     0x1d552, 0x1d6a5; 0x1d6a8, 0x1d6c0; 0x1d6c2, 0x1d6da; 0x1d6dc, 0x1d6fa; 0x1d6fc, 0x1d714;
     0x1d716, 0x1d734; 0x1d736, 0x1d74e; 0x1d750, 0x1d76e; 0x1d770, 0x1d788; 0x1d78a, 0x1d7a8;
     0x1d7aa, 0x1d7c2; 0x1d7c4, 0x1d7cb; 0x1df00, 0x1df09; 0x1df0a, 0x1df0a; 0x1df0b, 0x1df1e;
     0x1e100, 0x1e12c; 0x1e137, 0x1e13d; 0x1e14e, 0x1e14e; 0x1e290, 0x1e2ad; 0x1e2c0, 0x1e2eb;
     0x1e7e0, 0x1e7e6; 0x1e7e8, 0x1e7eb; 0x1e7ed, 0x1e7ee; 0x1e7f0, 0x1e7fe; 0x1e800, 0x1e8c4;
     0x1e900, 0x1e943; 0x1e94b, 0x1e94b; 0x1ee00, 0x1ee03; 0x1ee05, 0x1ee1f; 0x1ee21, 0x1ee22;
     0x1ee24, 0x1ee24; 0x1ee27, 0x1ee27; 0x1ee29, 0x1ee32; 0x1ee34, 0x1ee37; 0x1ee39, 0x1ee39;
     0x1ee3b, 0x1ee3b; 0x1ee42, 0x1ee42; 0x1ee47, 0x1ee47; 0x1ee49, 0x1ee49; 0x1ee4b, 0x1ee4b;
     0x1ee4d, 0x1ee4f; 0x1ee51, 0x1ee52; 0x1ee54, 0x1ee54; 0x1ee57, 0x1ee57; 0x1ee59, 0x1ee59;
     0x1ee5b, 0x1ee5b; 0x1ee5d, 0x1ee5d; 0x1ee5f, 0x1ee5f; 0x1ee61, 0x1ee62; 0x1ee64, 0x1ee64;
     0x1ee67, 0x1ee6a; 0x1ee6c, 0x1ee72; 0x1ee74, 0x1ee77; 0x1ee79, 0x1ee7c; 0x1ee7e, 0x1ee7e;
     0x1ee80, 0x1ee89; 0x1ee8b, 0x1ee9b; 0x1eea1, 0x1eea3; 0x1eea5, 0x1eea9; 0x1eeab, 0x1eebb;
     0x20000, 0x2a6df; 0x2a700, 0x2b738; 0x2b740, 0x2b81d; 0x2b820, 0x2cea1; 0x2ceb0, 0x2ebe0;
     0x30000, 0x3134a; 0x2f800, 0x2fa1d]

  let white_space =
    [0x9, 0xd; 0x20, 0x20; 0x85, 0x85; 0xa0, 0xa0; 0x1680, 0x1680;
     0x2000, 0x200a; 0x2028, 0x2028; 0x2029, 0x2029; 0x202f, 0x202f; 0x205f, 0x205f;
     0x3000, 0x3000]

  let list = [
    ("id_start", id_start);
    ("white_space", white_space)
  ]

end


(* Decision tree for partitions *)

let default_loc = Location.none

let lident_loc ~loc s = {
  loc;
  txt= lident s
}

type decision_tree =
  | Lte of int * decision_tree * decision_tree
  | Table of int * int array
  | Return of int

let rec simplify_decision_tree ( x : decision_tree) =
  match x with
  | Table _ | Return _ -> x
  | Lte (_, (Return a as l), Return b) when a = b -> l
  | Lte (i, l, r) ->
    let l = simplify_decision_tree l in
    let r = simplify_decision_tree r in
    match l, r with
    | Return a, Return b when a = b -> l
    | _ -> Lte (i, l,r)

let decision l =
  let l = List.map (fun (a, b, i) -> (a, b, Return i)) l in
  let rec merge2 = function
    | (a1, b1, d1) :: (a2, b2, d2) :: rest ->
        let x =
          if b1 + 1 = a2 then d2
          else Lte (a2 - 1, Return (-1), d2)
        in
        (a1, b2, Lte (b1, d1, x)) :: merge2 rest
    | rest -> rest
  in
  let rec aux = function
    | [(a, b, d)] -> Lte (a - 1, Return (-1), Lte (b, d, Return (-1)))
    | [] -> Return (-1)
    | l -> aux (merge2 l)
  in
  aux l

let limit = 8192

let decision_table l =
  let rec aux m accu = function
    | ((a, b, i) as x)::rem when b < limit && i < 255->
        aux (min a m) (x :: accu) rem
    | rem -> m, accu, rem
  in
  let (min, table, rest) = aux max_int [] l in
  match table with
  | [] -> decision l
  | [(min, max, i)] ->
      Lte (min - 1, Return (-1), (Lte (max, Return i, decision rest)))
  | (_, max, _) :: _ ->
      let arr = Array.make (max - min + 1) 0 in
      let set (a, b, i) = for j = a to b do arr.(j - min) <- i + 1 done in
      List.iter set table;
      Lte (min - 1, Return (-1), Lte (max, Table (min, arr), decision rest))

let rec simplify min max = function
  | Lte (i,yes,no) ->
      if i >= max then simplify min max yes
      else if i < min then simplify min max no
      else Lte (i, simplify min i yes, simplify (i+1) max no)
  | x -> x

let segments_of_partition p =
  let seg = ref [] in
  Array.iteri
    (fun i c -> List.iter (fun (a, b) -> seg := (a, b, i) :: !seg) c)
    p;
  List.sort (fun (a1,_,_) (a2,_,_) -> compare a1 a2) !seg

let decision_table p =
  simplify (-1) (Cset.max_code) (decision_table (segments_of_partition p))


(* Helpers to build AST *)

let appfun s l =
  let loc = default_loc in
  eapply ~loc (evar ~loc s) l

let glb_value name def =
  let loc = default_loc in
  pstr_value ~loc Nonrecursive [value_binding ~loc ~pat:(pvar ~loc name) ~expr:def]

(* Named regexps *)

module StringMap = Map.Make(struct
  type t = string
  let compare = compare
end)

let builtin_regexps =
  List.fold_left (fun acc (n, c) -> StringMap.add n (Flow_sedlex.chars c) acc)
    StringMap.empty
    ([
     "any", Cset.any;
     "eof", Cset.eof] @
     UnicodeProperties.list)

(* Tables (indexed mapping: codepoint -> next state) *)

let tables = Hashtbl.create 31
let table_counter = ref 0
let get_tables () = Hashtbl.fold (fun key x accu -> (x, key) :: accu) tables []

let table_name x =
  try Hashtbl.find tables x
  with Not_found ->
    incr table_counter;
    let s = Printf.sprintf "__sedlex_table_%i" !table_counter in
    Hashtbl.add tables x s;
    s

let table (name, v) =
  let n = Array.length v in
  let s = Bytes.create n in
  for i = 0 to n - 1 do Bytes.set s i (Char.chr v.(i)) done;
  glb_value name (estring ~loc:default_loc (Bytes.to_string s))

(* Partition (function: codepoint -> next state) *)

let partitions = Hashtbl.create 31
let partition_counter = ref 0
let get_partitions () = Hashtbl.fold (fun key x accu -> (x, key) :: accu) partitions []

let partition_name x =
  try Hashtbl.find partitions x
  with Not_found ->
    incr partition_counter;
    let s = Printf.sprintf "__sedlex_partition_%i" !partition_counter in
    Hashtbl.add partitions x s;
    s

(* We duplicate the body for the EOF (-1) case rather than creating
   an interior utility function. *)
let partition (name, p) =
  let loc = default_loc in
  let rec gen_tree = function
    | Lte (i, yes, no) ->
        [%expr if c <= [%e eint ~loc i] then [%e gen_tree yes] else [%e gen_tree no]]
    | Return i -> eint ~loc:default_loc i
    | Table (offset, t) ->
              let c = if offset = 0 then [%expr c] else [%expr c - [%e eint ~loc offset]] in
        [%expr Char.code (String.unsafe_get [%e evar ~loc (table_name t)] [%e c]) - 1]
  in
  let body = gen_tree (simplify_decision_tree (decision_table p)) in
  glb_value name
    [%expr fun c ->
      [%e body]
    ]

(* Code generation for the automata *)

let best_final final =
  let fin = ref None in
  for i = Array.length final - 1 downto 0 do
    if final.(i) then fin := Some i
  done;
  !fin

let state_fun state = Printf.sprintf "__sedlex_state_%i" state

let call_state lexbuf auto state =
  let (trans, final) = auto.(state) in
  if Array.length trans = 0
  then match best_final final with
  | Some i -> eint ~loc:default_loc i
  | None -> assert false
  else appfun (state_fun state) [evar ~loc:default_loc lexbuf]

let gen_state lexbuf auto i (trans, final) =
  let loc = default_loc in
  let partition = Array.map fst trans in
  let cases = Array.mapi (fun i (_, j) -> case ~lhs:(pint ~loc i) ~guard:None ~rhs:(call_state lexbuf auto j)) trans in
  let cases = Array.to_list cases in
  let body () =
    pexp_match ~loc
      (appfun (partition_name partition) [[%expr Sedlexing.__private__next_int [%e evar ~loc lexbuf]]])
      (cases @ [case ~lhs:[%pat? _] ~guard:None ~rhs:[%expr Sedlexing.backtrack [%e evar ~loc lexbuf]]])
  in
  let ret body = [ value_binding ~loc ~pat:(pvar ~loc (state_fun i)) ~expr:(pexp_function ~loc [case ~lhs:(pvar ~loc lexbuf) ~guard:None ~rhs:body]) ] in
  match best_final final with
    | None -> ret (body ())
    | Some _ when Array.length trans = 0 -> []
    | Some i -> ret [%expr Sedlexing.mark [%e evar ~loc lexbuf] [%e eint ~loc i]; [%e body ()]]

let gen_recflag auto =
  (* The generated function is not recursive if the transitions end
     in states with no further transitions. *)
  try
    Array.iter
      (fun (trans_i, _) ->
        Array.iter
          (fun (_, j) ->
            let (trans_j, _) = auto.(j) in
            if Array.length trans_j > 0 then raise Exit)
          trans_i)
      auto;
    Nonrecursive
  with
    Exit -> Recursive

let gen_definition lexbuf l error =
  let loc = default_loc in
  let brs = Array.of_list l in
  let auto = Flow_sedlex.compile (Array.map fst brs) in
  let cases = Array.to_list (Array.mapi (fun i (_, e) -> case ~lhs:(pint ~loc i) ~guard:None ~rhs:e) brs) in
  let states = Array.mapi (gen_state lexbuf auto) auto in
  let states = List.flatten (Array.to_list states) in
  pexp_let ~loc (gen_recflag auto) states
    (pexp_sequence ~loc
       [%expr Sedlexing.start [%e evar ~loc lexbuf]]
       (pexp_match ~loc (appfun (state_fun 0) [evar ~loc lexbuf])
          (cases @ [case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:error])
       )
    )

(* Lexer specification parser *)

let codepoint i =
  if i < 0 || i > Cset.max_code then
    failwith (Printf.sprintf "Invalid Unicode code point: %i" i);
  i

let regexp_for_char c =
  Flow_sedlex.chars (Cset.singleton (Char.code c))

let regexp_for_string s =
  let rec aux n =
    if n = String.length s then Flow_sedlex.eps
    else
      Flow_sedlex.seq (regexp_for_char s.[n]) (aux (succ n))
  in aux 0

let err loc s =
  raise (Location.Error (Location.Error.createf ~loc "Sedlex: %s" s))

let rec repeat r = function
  | 0, 0 -> Flow_sedlex.eps
  | 0, m -> Flow_sedlex.alt Flow_sedlex.eps (Flow_sedlex.seq r (repeat r (0, m - 1)))
  | n, m -> Flow_sedlex.seq r (repeat r (n - 1, m - 1))

let regexp_of_pattern env =
  let rec char_pair_op func name p tuple = (* Construct something like Sub(a,b) *)
    match tuple with
      | Some {ppat_desc=Ppat_tuple (p0 :: p1 :: [])} ->
        begin match func (aux p0) (aux p1) with
        | Some r -> r
        | None ->
          err p.ppat_loc @@
            "the "^name^" operator can only applied to single-character length regexps"
        end
      | _ -> err p.ppat_loc @@ "the "^name^" operator requires two arguments, like "^name^"(a,b)"
  and aux p = (* interpret one pattern node *)
    match p.ppat_desc with
    | Ppat_or (p1, p2) -> Flow_sedlex.alt (aux p1) (aux p2)
    | Ppat_tuple (p :: pl) ->
        List.fold_left (fun r p -> Flow_sedlex.seq r (aux p))
          (aux p)
          pl
    | Ppat_construct ({txt = Lident "Star"}, Some p) ->
        Flow_sedlex.rep (aux p)
    | Ppat_construct ({txt = Lident "Plus"}, Some p) ->
        Flow_sedlex.plus (aux p)
    | Ppat_construct
        ({txt = Lident "Rep"},
         Some {ppat_desc=Ppat_tuple[p0; {ppat_desc=Ppat_constant (i1 as i2)|Ppat_interval(i1, i2)}]}) ->
         begin match i1, i2 with
         | Pconst_integer(i1,_), Pconst_integer(i2,_) ->
             let i1 = int_of_string i1 in
             let i2 = int_of_string i2 in
             if 0 <= i1 && i1 <= i2 then repeat (aux p0) (i1, i2)
             else err p.ppat_loc "Invalid range for Rep operator"
         | _ ->
             err p.ppat_loc "Rep must take an integer constant or interval"
         end
    | Ppat_construct ({txt = Lident "Rep"}, _) ->
        err p.ppat_loc "the Rep operator takes 2 arguments"
    | Ppat_construct ({txt = Lident "Opt"}, Some p) ->
        Flow_sedlex.alt Flow_sedlex.eps (aux p)
    | Ppat_construct ({txt = Lident "Compl"}, arg) ->
        begin match arg with
        | Some p0 ->
            begin match Flow_sedlex.compl (aux p0) with
            | Some r -> r
            | None ->
              err p.ppat_loc
                "the Compl operator can only applied to a single-character length regexp"
            end
        | _ -> err p.ppat_loc "the Compl operator requires an argument"
        end
    | Ppat_construct ({ txt = Lident "Sub" }, arg) ->
        char_pair_op Flow_sedlex.subtract "Sub" p arg
    | Ppat_construct ({ txt = Lident "Intersect" }, arg) ->
        char_pair_op Flow_sedlex.intersection "Intersect" p arg
    | Ppat_construct ({txt = Lident "Chars"}, arg) ->
        let const = match arg with
          | Some {ppat_desc=Ppat_constant const} ->
              Some (const)
          | _ -> None
        in
        begin match const with
        | Some (Pconst_string(s,_, _))->
            let c = ref Cset.empty in
            for i = 0 to String.length s - 1 do
              c := Cset.union !c (Cset.singleton (Char.code s.[i]))
            done;
            Flow_sedlex.chars !c
        | _ -> err p.ppat_loc "the Chars operator requires a string argument"
        end
    | Ppat_interval (i_start, i_end) ->
        begin match  i_start, i_end with
          | Pconst_char c1, Pconst_char c2 -> Flow_sedlex.chars (Cset.interval (Char.code c1) (Char.code c2))
          | Pconst_integer(i1,_), Pconst_integer(i2,_) ->
              Flow_sedlex.chars (Cset.interval (codepoint (int_of_string i1)) (codepoint (int_of_string i2)))
          | _ -> err p.ppat_loc "this pattern is not a valid interval regexp"
        end
    | Ppat_constant (const) ->
        begin match const with
          | Pconst_string (s, _, _) -> regexp_for_string s
          | Pconst_char c -> regexp_for_char c
          | Pconst_integer(i,_) -> Flow_sedlex.chars (Cset.singleton (codepoint (int_of_string i)))
          | _ -> err p.ppat_loc "this pattern is not a valid regexp"
        end
    | Ppat_var {txt=x} ->
        begin try StringMap.find x env
        with Not_found ->
          err p.ppat_loc (Printf.sprintf "unbound regexp %s" x)
        end
    | _ ->
      err p.ppat_loc "this pattern is not a valid regexp"
  in
  aux


let previous = ref []
let regexps = ref []
let should_set_cookies = ref false

let mapper =
  object(this)
    inherit Ast_traverse.map as super

    val env = builtin_regexps

    method define_regexp name p =
      {< env = StringMap.add name (regexp_of_pattern env p) env >}

    method! expression e =
      match e with
      | [%expr [%sedlex [%e? {pexp_desc=Pexp_match (lexbuf, cases)}]]] ->
            let lexbuf =
              match lexbuf with
              | {pexp_desc=Pexp_ident{txt=Lident lexbuf}} -> lexbuf
              | _ ->
                err lexbuf.pexp_loc "the matched expression must be a single identifier"
            in
            let cases = List.rev cases in
            let error =
              match List.hd cases with
              | {pc_lhs = [%pat? _]; pc_rhs = e; pc_guard = None} -> super # expression e
              | {pc_lhs = p} ->
                err p.ppat_loc "the last branch must be a catch-all error case"
            in
            let cases = List.rev (List.tl cases) in
            let cases =
              List.map
                (function
                  | {pc_lhs = p; pc_rhs = e; pc_guard = None} -> regexp_of_pattern env p, super # expression e
                  | {pc_guard = Some e} ->
                    err e.pexp_loc "'when' guards are not supported"
                ) cases
            in
            gen_definition lexbuf cases error
      | [%expr let [%p? {ppat_desc=Ppat_var{txt=name}}] = [%sedlex.regexp? [%p? p]] in [%e? body]] ->
          (this # define_regexp name p) # expression body
      | [%expr [%sedlex [%e? _]]] ->
        err e.pexp_loc "the %sedlex extension is only recognized on match expressions"
      | _ -> super # expression e


    val toplevel = true

    method structure_with_regexps l =
      let mapper = ref this in
      let regexps = ref [] in
      let l = List.concat
        (List.map
           (function
             | [%stri let [%p? {ppat_desc=Ppat_var{txt=name}}] = [%sedlex.regexp? [%p? p]]] as i ->
               regexps := i :: !regexps;
               mapper := !mapper # define_regexp name p;
               []
             | i ->
               [ !mapper # structure_item i ]
         ) l) in
      (l, List.rev !regexps)

    method! structure l =
      if toplevel then
        let sub = {< toplevel = false >} in
        let l, regexps' = sub # structure_with_regexps (!previous @ l) in
        let parts = List.map partition (get_partitions ()) in
        let tables = List.map table (get_tables ()) in
        regexps := regexps';
        should_set_cookies := true;
        tables @ parts @ l
      else
        fst (this # structure_with_regexps l)

 end

let pre_handler cookies =
  previous :=
    match Driver.Cookies.get cookies "sedlex.regexps" Ast_pattern.__ with
    | Some {pexp_desc = Pexp_extension (_, PStr l)} -> l
    | Some _ -> assert false
    | None -> []

let post_handler cookies =
  if !should_set_cookies then
    let loc = default_loc in
    Driver.Cookies.set cookies "sedlex.regexps" (pexp_extension ~loc ( {loc; txt="regexps"}, PStr !regexps))


let extensions =
  [Extension.declare
    "sedlex"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc:_ ~path:_ expr -> mapper # expression expr);
  ]

let () =
  Driver.Cookies.add_handler pre_handler;
  Driver.Cookies.add_post_handler post_handler;
  Driver.register_transformation "sedlex"  ~impl:(mapper # structure)