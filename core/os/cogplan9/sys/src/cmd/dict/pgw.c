#include <u.h>
#include <libc.h>
#include <bio.h>
#include "dict.h"
enum {
Buflen=1000,
Maxaux=5,
};
enum {
B,
Blockquote,
Br,
Cd,
Col,
Def,
Hw,
I,
P,
Pos,
Sn,
U,
Wf,
Ntag
};
static Assoc tagtab[] = {
{"b",			B},
{"blockquote",	Blockquote},
{"BR",		Br},
{"cd",		Cd},
{"col",		Col},
{"def",		Def},
{"hw",		Hw},
{"i",			I},
{"p",			P},
{"pos",		Pos},
{"sn",		Sn},
{"u",			U},
{"wf",		Wf},
};
enum {
Cols,
Num,
St,
Naux
};
static Assoc auxtab[] = {
{"cols",	Cols},
{"num",		Num},
{"st",		St}
};
static Assoc spectab[] = {
{"3on4",	L'¾'},
{"AElig",		L'Æ'},
{"Aacute",	L'Á'},
{"Aang",	L'Å'},
{"Abarab",	L'Ā'},
{"Acirc",	L'Â'},
{"Agrave",	L'À'},
{"Alpha",	L'Α'},
{"Amacr",	L'Ā'},
{"Asg",		L'Ʒ'},
{"Auml",	L'Ä'},
{"Beta",	L'Β'},
{"Cced",	L'Ç'},
{"Chacek",	L'Č'},
{"Chi",		L'Χ'},
{"Chirho",	L'☧'},
{"Csigma",	L'Ϛ'},
{"Delta",	L'Δ'},
{"Eacute",	L'É'},
{"Ecirc",	L'Ê'},
{"Edh",		L'Ð'},
{"Epsilon",	L'Ε'},
{"Eta",		L'Η'},
{"Gamma",	L'Γ'},
{"Iacute",	L'Í'},
{"Icirc",	L'Î'},
{"Imacr",	L'Ī'},
{"Integ",	L'∫'},
{"Iota",	L'Ι'},
{"Kappa",	L'Κ'},
{"Koppa",	L'Ϟ'},
{"Lambda",	L'Λ'},
{"Lbar",	L'Ł'},
{"Mu",		L'Μ'},
{"Naira",	L'N'},
{"Nplus",	L'N'},
{"Ntilde",	L'Ñ'},
{"Nu",		L'Ν'},
{"Oacute",	L'Ó'},
{"Obar",	L'Ø'},
{"Ocirc",	L'Ô'},
{"Oe",		L'Œ'},
{"Omega",	L'Ω'},
{"Omicron",	L'Ο'},
{"Ouml",	L'Ö'},
{"Phi",		L'Φ'},
{"Pi",		L'Π'},
{"Psi",		L'Ψ'},
{"Rho",		L'Ρ'},
{"Sacute",	L'Ś'},
{"Sigma",	L'Σ'},
{"Summ",	L'∑'},
{"Tau",		L'Τ'},
{"Th",		L'Þ'},
{"Theta",	L'Θ'},
{"Tse",		L'Ц'},
{"Uacute",	L'Ú'},
{"Ucirc",	L'Û'},
{"Upsilon",	L'Υ'},
{"Uuml",	L'Ü'},
{"Wyn",		L'ƿ'},
{"Xi",		L'Ξ'},
{"Ygh",		L'Ʒ'},
{"Zeta",	L'Ζ'},
{"Zh",		L'Ʒ'},
{"a",		L'a'},
{"aacute",	L'á'},
{"aang",	L'å'},
{"aasper",	MAAS},
{"abreve",	L'ă'},
{"acirc",	L'â'},
{"acute",		LACU},
{"aelig",		L'æ'},
{"agrave",	L'à'},
{"ahook",	L'ą'},
{"alenis",	MALN},
{"alpha",	L'α'},
{"amacr",	L'ā'},
{"amp",		L'&'},
{"and",		MAND},
{"ang",		LRNG},
{"angle",	L'∠'},
{"ankh",	L'☥'},
{"ante",	L'a'},
{"aonq",	MAOQ},
{"appreq",	L'≃'},
{"aquar",	L'♒'},
{"arDadfull",	L'ض'},
{"arHa",	L'ح'},
{"arTa",	L'ت'},
{"arain",	L'ع'},
{"arainfull",	L'ع'},
{"aralif",	L'ا'},
{"arba",	L'ب'},
{"arha",	L'ه'},
{"aries",	L'♈'},
{"arnun",	L'ن'},
{"arnunfull",	L'ن'},
{"arpa",	L'ه'},
{"arqoph",	L'ق'},
{"arshinfull",	L'ش'},
{"arta",	L'ت'},
{"artafull",	L'ت'},
{"artha",	L'ث'},
{"arwaw",	L'و'},
{"arya",	L'ي'},
{"aryafull",	L'ي'},
{"arzero",	L'٠'},
{"asg",		L'ʒ'},
{"asper",	LASP},
{"assert",	L'⊢'},
{"astm",	L'⁂'},
{"at",		L'@'},
{"atilde",	L'ã'},
{"auml",	L'ä'},
{"ayin",	L'ع'},
{"b1",		L'-'},
{"b2",		L'='},
{"b3",		L'≡'},
{"bbar",	L'ƀ'},
{"beta",	L'β'},
{"bigobl",	L'/'},
{"blC",		L'C'},
{"blJ",		L'J'},
{"blU",		L'U'},
{"blb",		L'b'},
{"blozenge",	L'◊'},
{"bly",		L'y'},
{"bra",		MBRA},
{"brbl",	LBRB},
{"breve",	LBRV},
{"bslash",	L'\\'},
{"bsquare",	L'■'},
{"btril",	L'◀'},
{"btrir",	L'▶'},
{"c",		L'c'},
{"cab",		L'〉'},
{"cacute",	L'ć'},
{"canc",	L'♋'},
{"capr",	L'♑'},
{"caret",	L'^'},
{"cb",		L'}'},
{"cbigb",	L'}'},
{"cbigpren",	L')'},
{"cbigsb",	L']'},
{"cced",	L'ç'},
{"cdil",	LCED},
{"cdsb",	L'〛'},
{"cent",	L'¢'},
{"chacek",	L'č'},
{"chi",		L'χ'},
{"circ",	LRNG},
{"circa",	L'c'},
{"circbl",	L'̥'},
{"circle",	L'○'},
{"circledot",	L'⊙'},
{"click",	L'ʖ'},
{"club",	L'♣'},
{"comtime",	L'C'},
{"conj",	L'☌'},
{"cprt",	L'©'},
{"cq",		L'\''},
{"cqq",		L'”'},
{"cross",	L'✠'},
{"crotchet",	L'♩'},
{"csb",		L']'},
{"ctilde",	L'c'},
{"ctlig",	MLCT},
{"cyra",	L'а'},
{"cyre",	L'е'},
{"cyrhard",	L'ъ'},
{"cyrjat",	L'ѣ'},
{"cyrm",	L'м'},
{"cyrn",	L'н'},
{"cyrr",	L'р'},
{"cyrsoft",	L'ь'},
{"cyrt",	L'т'},
{"cyry",	L'ы'},
{"dag",		L'†'},
{"dbar",	L'đ'},
{"dblar",	L'⇋'},
{"dblgt",	L'≫'},
{"dbllt",	L'≪'},
{"dced",	L'd'},
{"dd",		MDD},
{"ddag",	L'‡'},
{"ddd",		MDDD},
{"decr",	L'↓'},
{"deg",		L'°'},
{"dele",	L'd'},
{"delta",	L'δ'},
{"descnode",	L'☋'},
{"diamond",	L'♢'},
{"digamma",	L'ϝ'},
{"div",		L'÷'},
{"dlessi",	L'ı'},
{"dlessj1",	L'j'},
{"dlessj2",	L'j'},
{"dlessj3",	L'j'},
{"dollar",	L'$'},
{"dotab",	LDOT},
{"dotbl",	LDTB},
{"drachm",	L'ʒ'},
{"dubh",	L'-'},
{"eacute",	L'é'},
{"earth",	L'♁'},
{"easper",	MEAS},
{"ebreve",	L'ĕ'},
{"ecirc",	L'ê'},
{"edh",		L'ð'},
{"egrave",	L'è'},
{"ehacek",	L'ě'},
{"ehook",	L'ę'},
{"elem",	L'∊'},
{"elenis",	MELN},
{"em",		L'—'},
{"emacr",	L'ē'},
{"emem",	MEMM},
{"en",		L'–'},
{"epsilon",	L'ε'},
{"equil",	L'⇋'},
{"ergo",	L'∴'},
{"es",		MES},
{"eszett",	L'ß'},
{"eta",		L'η'},
{"eth",		L'ð'},
{"euml",	L'ë'},
{"expon",	L'↑'},
{"fact",	L'!'},
{"fata",	L'ɑ'},
{"fatpara",	L'¶'},
{"female",	L'♀'},
{"ffilig",	MLFFI},
{"fflig",	MLFF},
{"ffllig",	MLFFL},
{"filig",	MLFI},
{"flat",	L'♭'},
{"fllig",	MLFL},
{"frE",		L'E'},
{"frL",		L'L'},
{"frR",		L'R'},
{"frakB",	L'B'},
{"frakG",	L'G'},
{"frakH",	L'H'},
{"frakI",	L'I'},
{"frakM",	L'M'},
{"frakU",	L'U'},
{"frakX",	L'X'},
{"frakY",	L'Y'},
{"frakh",	L'h'},
{"frbl",	LFRB},
{"frown",	LFRN},
{"fs",		L' '},
{"fsigma",	L'ς'},
{"gAacute",	L'Á'},
{"gaacute",	L'α'},
{"gabreve",	L'α'},
{"gafrown",	L'α'},
{"gagrave",	L'α'},
{"gamacr",	L'α'},
{"gamma",	L'γ'},
{"gauml",	L'α'},
{"ge",		L'≧'},
{"geacute",	L'ε'},
{"gegrave",	L'ε'},
{"ghacute",	L'η'},
{"ghfrown",	L'η'},
{"ghgrave",	L'η'},
{"ghmacr",	L'η'},
{"giacute",	L'ι'},
{"gibreve",	L'ι'},
{"gifrown",	L'ι'},
{"gigrave",	L'ι'},
{"gimacr",	L'ι'},
{"giuml",	L'ι'},
{"glagjat",	L'ѧ'},
{"glots",	L'ˀ'},
{"goacute",	L'ο'},
{"gobreve",	L'ο'},
{"grave",	LGRV},
{"gt",		L'>'},
{"guacute",	L'υ'},
{"gufrown",	L'υ'},
{"gugrave",	L'υ'},
{"gumacr",	L'υ'},
{"guuml",	L'υ'},
{"gwacute",	L'ω'},
{"gwfrown",	L'ω'},
{"gwgrave",	L'ω'},
{"hacek",	LHCK},
{"halft",	L'⌈'},
{"hash",	L'#'},
{"hasper",	MHAS},
{"hatpath",	L'ֲ'},
{"hatqam",	L'ֳ'},
{"hatseg",	L'ֱ'},
{"hbar",	L'ħ'},
{"heart",	L'♡'},
{"hebaleph",	L'א'},
{"hebayin",	L'ע'},
{"hebbet",	L'ב'},
{"hebbeth",	L'ב'},
{"hebcheth",	L'ח'},
{"hebdaleth",	L'ד'},
{"hebgimel",	L'ג'},
{"hebhe",	L'ה'},
{"hebkaph",	L'כ'},
{"heblamed",	L'ל'},
{"hebmem",	L'מ'},
{"hebnun",	L'נ'},
{"hebnunfin",	L'ן'},
{"hebpe",	L'פ'},
{"hebpedag",	L'ף'},
{"hebqoph",	L'ק'},
{"hebresh",	L'ר'},
{"hebshin",	L'ש'},
{"hebtav",	L'ת'},
{"hebtsade",	L'צ'},
{"hebwaw",	L'ו'},
{"hebyod",	L'י'},
{"hebzayin",	L'ז'},
{"hgz",		L'ʒ'},
{"hireq",	L'ִ'},
{"hlenis",	MHLN},
{"hook",	LOGO},
{"horizE",	L'E'},
{"horizP",	L'P'},
{"horizS",	L'∽'},
{"horizT",	L'⊣'},
{"horizb",	L'{'},
{"ia",		L'α'},
{"iacute",	L'í'},
{"iasper",	MIAS},
{"ib",		L'β'},
{"ibar",	L'ɨ'},
{"ibreve",	L'ĭ'},
{"icirc",	L'î'},
{"id",		L'δ'},
{"ident",	L'≡'},
{"ie",		L'ε'},
{"ifilig",	MLFI},
{"ifflig",	MLFF},
{"ig",		L'γ'},
{"igrave",	L'ì'},
{"ih",		L'η'},
{"ii",		L'ι'},
{"ik",		L'κ'},
{"ilenis",	MILN},
{"imacr",	L'ī'},
{"implies",	L'⇒'},
{"index",	L'☞'},
{"infin",	L'∞'},
{"integ",	L'∫'},
{"intsec",	L'∩'},
{"invpri",	L'ˏ'},
{"iota",	L'ι'},
{"iq",		L'ψ'},
{"istlig",	MLST},
{"isub",	L'ϵ'},
{"iuml",	L'ï'},
{"iz",		L'ζ'},
{"jup",		L'♃'},
{"kappa",	L'κ'},
{"koppa",	L'ϟ'},
{"lambda",	L'λ'},
{"lar",		L'←'},
{"lbar",	L'ł'},
{"le",		L'≦'},
{"lenis",	LLEN},
{"leo",		L'♌'},
{"lhalfbr",	L'⌈'},
{"lhshoe",	L'⊃'},
{"libra",	L'♎'},
{"llswing",	MLLS},
{"lm",		L'ː'},
{"logicand",	L'∧'},
{"logicor",	L'∨'},
{"longs",	L'ʃ'},
{"lrar",	L'↔'},
{"lt",		L'<'},
{"ltappr",	L'≾'},
{"ltflat",	L'∠'},
{"lumlbl",	L'l'},
{"mac",		LMAC},
{"male",	L'♂'},
{"mc",		L'c'},
{"merc",	L'☿'},
{"min",		L'−'},
{"moonfq",	L'☽'},
{"moonlq",	L'☾'},
{"msylab",	L'm'},
{"mu",		L'μ'},
{"nacute",	L'ń'},
{"natural",	L'♮'},
{"neq",		L'≠'},
{"nfacute",	L'′'},
{"nfasper",	L'ʽ'},
{"nfbreve",	L'˘'},
{"nfced",	L'¸'},
{"nfcirc",	L'ˆ'},
{"nffrown",	L'⌢'},
{"nfgra",	L'ˋ'},
{"nfhacek",	L'ˇ'},
{"nfmac",	L'¯'},
{"nftilde",	L'˜'},
{"nfuml",	L'¨'},
{"ng",		L'ŋ'},
{"not",		L'¬'},
{"notelem",	L'∉'},
{"ntilde",	L'ñ'},
{"nu",		L'ν'},
{"oab",		L'〈'},
{"oacute",	L'ó'},
{"oasper",	MOAS},
{"ob",		L'{'},
{"obar",	L'ø'},
{"obigb",	L'{'},
{"obigpren",	L'('},
{"obigsb",	L'['},
{"obreve",	L'ŏ'},
{"ocirc",	L'ô'},
{"odsb",	L'〚'},
{"oelig",		L'œ'},
{"oeamp",	L'&'},
{"ograve",	L'ò'},
{"ohook",	L'o'},
{"olenis",	MOLN},
{"omacr",	L'ō'},
{"omega",	L'ω'},
{"omicron",	L'ο'},
{"ope",		L'ɛ'},
{"opp",		L'☍'},
{"oq",		L'`'},
{"oqq",		L'“'},
{"or",		MOR},
{"osb",		L'['},
{"otilde",	L'õ'},
{"ouml",	L'ö'},
{"ounce",	L'℥'},
{"ovparen",	L'⌢'},
{"p",		L'′'},
{"pa",		L'∂'},
{"page",	L'P'},
{"pall",	L'ʎ'},
{"paln",	L'ɲ'},
{"par",		PAR},
{"para",	L'¶'},
{"pbar",	L'p'},
{"per",		L'℘'},
{"phi",		L'φ'},
{"phi2",	L'ϕ'},
{"pi",		L'π'},
{"pisces",	L'♓'},
{"planck",	L'ħ'},
{"plantinJ",	L'J'},
{"pm",		L'±'},
{"pmil",	L'‰'},
{"pp",		L'″'},
{"ppp",		L'‴'},
{"prop",	L'∝'},
{"psi",		L'ψ'},
{"pstlg",	L'£'},
{"q",		L'?'},
{"qamets",	L'ֳ'},
{"quaver",	L'♪'},
{"rar",		L'→'},
{"rasper",	MRAS},
{"rdot",	L'·'},
{"recipe",	L'℞'},
{"reg",		L'®'},
{"revC",	L'Ɔ'},
{"reva",	L'ɒ'},
{"revc",	L'ɔ'},
{"revope",	L'ɜ'},
{"revr",	L'ɹ'},
{"revsc",	L'˒'},
{"revv",	L'ʌ'},
{"rfa",		L'o'},
{"rhacek",	L'ř'},
{"rhalfbr",	L'⌉'},
{"rho",		L'ρ'},
{"rhshoe",	L'⊂'},
{"rlenis",	MRLN},
{"rsylab",	L'r'},
{"runash",	L'F'},
{"rvow",	L'˔'},
{"sacute",	L'ś'},
{"sagit",	L'♐'},
{"sampi",	L'ϡ'},
{"saturn",	L'♄'},
{"sced",	L'ş'},
{"schwa",	L'ə'},
{"scorpio",	L'♏'},
{"scrA",	L'A'},
{"scrC",	L'C'},
{"scrE",	L'E'},
{"scrF",	L'F'},
{"scrI",	L'I'},
{"scrJ",	L'J'},
{"scrL",	L'L'},
{"scrO",	L'O'},
{"scrP",	L'P'},
{"scrQ",	L'Q'},
{"scrS",	L'S'},
{"scrT",	L'T'},
{"scrb",	L'b'},
{"scrd",	L'd'},
{"scrh",	L'h'},
{"scrl",	L'l'},
{"scruple",	L'℈'},
{"sdd",		L'ː'},
{"sect",	L'§'},
{"semE",	L'∃'},
{"sh",		L'ʃ'},
{"shacek",	L'š'},
{"sharp",	L'♯'},
{"sheva",	L'ְ'},
{"shti",	L'ɪ'},
{"shtsyll",	L'∪'},
{"shtu",	L'ʊ'},
{"sidetri",	L'⊲'},
{"sigma",	L'σ'},
{"since",	L'∵'},
{"slge",	L'≥'},
{"slle",	L'≤'},
{"sm",		L'ˈ'},
{"smm",		L'ˌ'},
{"spade",	L'♠'},
{"sqrt",	L'√'},
{"square",	L'□'},
{"ssChi",	L'Χ'},
{"ssIota",	L'Ι'},
{"ssOmicron",	L'Ο'},
{"ssPi",	L'Π'},
{"ssRho",	L'Ρ'},
{"ssSigma",	L'Σ'},
{"ssTau",	L'Τ'},
{"star",	L'*'},
{"stlig",	MLST},
{"sup2",	L'⁲'},
{"supgt",	L'˃'},
{"suplt",	L'˂'},
{"sur",		L'ʳ'},
{"swing",	L'∼'},
{"tau",		L'τ'},
{"taur",	L'♉'},
{"th",		L'þ'},
{"thbar",	L'þ'},
{"theta",	L'θ'},
{"thinqm",	L'?'},
{"tilde",	LTIL},
{"times",	L'×'},
{"tri",		L'∆'},
{"trli",	L'‖'},
{"ts",		L' '},
{"uacute",	L'ú'},
{"uasper",	MUAS},
{"ubar",	L'u'},
{"ubreve",	L'ŭ'},
{"ucirc",	L'û'},
{"udA",		L'∀'},
{"udT",		L'⊥'},
{"uda",		L'ɐ'},
{"udh",		L'ɥ'},
{"udqm",	L'¿'},
{"udpsi",	L'⋔'},
{"udtr",	L'∇'},
{"ugrave",	L'ù'},
{"ulenis",	MULN},
{"umacr",	L'ū'},
{"uml",		LUML},
{"undl",	L'ˍ'},
{"union",	L'∪'},
{"upsilon",	L'υ'},
{"uuml",	L'ü'},
{"vavpath",	L'ו'},
{"vavsheva",	L'ו'},
{"vb",		L'|'},
{"vddd",	L'⋮'},
{"versicle2",	L'℣'},
{"vinc",	L'¯'},
{"virgo",	L'♍'},
{"vpal",	L'ɟ'},
{"vvf",		L'ɣ'},
{"wasper",	MWAS},
{"wavyeq",	L'≈'},
{"wlenis",	MWLN},
{"wyn",		L'ƿ'},
{"xi",		L'ξ'},
{"yacute",	L'ý'},
{"ycirc",	L'ŷ'},
{"ygh",		L'ʒ'},
{"ymacr",	L'y'},
{"yuml",	L'ÿ'},
{"zced",	L'z'},
{"zeta",	L'ζ'},
{"zh",		L'ʒ'},
{"zhacek",	L'ž'},
};
static Rune normtab[128] = {
NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,
NONE,	NONE,	' ',	NONE,	NONE,	NONE,	NONE,	NONE,
NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,
NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,
L' ',	L'!',	L'"',	L'#',	L'$',	L'%',	SPCS,	L'\'',
L'(',	L')',	L'*',	L'+',	L',',	L'-',	L'.',	L'/',
L'0',	L'1',	L'2',	L'3',	L'4',	L'5',	L'6',	L'7',
L'8',	L'9',	L':',	L';',	TAGS,	L'=',	TAGE,	L'?',
L'@',	L'A',	L'B',	L'C',	L'D',	L'E',	L'F',	L'G',
L'H',	L'I',	L'J',	L'K',	L'L',	L'M',	L'N',	L'O',
L'P',	L'Q',	L'R',	L'S',	L'T',	L'U',	L'V',	L'W',
L'X',	L'Y',	L'Z',	L'[',	L'\\',	L']',	L'^',	L'_',
L'`',	L'a',	L'b',	L'c',	L'd',	L'e',	L'f',	L'g',
L'h',	L'i',	L'j',	L'k',	L'l',	L'm',	L'n',	L'o',
L'p',	L'q',	L'r',	L's',	L't',	L'u',	L'v',	L'w',
L'x',	L'y',	L'z',	L'{',	L'|',	L'}',	L'~',	NONE,
};
static Rune phtab[128] = {
NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,
NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,
NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,
NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,
L' ',	L'!',	L'ˈ',	L'#',	L'$',	L'ˌ',	L'æ',	L'\'',
L'(',	L')',	L'*',	L'+',	L',',	L'-',	L'.',	L'/',
L'0',	L'1',	L'2',	L'ɜ',	L'4',	L'5',	L'6',	L'7',
L'8',	L'ø',	L'ː',	L';',	TAGS,	L'=',	TAGE,	L'?',
L'ə',	L'ɑ',	L'B',	L'C',	L'ð',	L'ɛ',	L'F',	L'G',
L'H',	L'ɪ',	L'J',	L'K',	L'L',	L'M',	L'ŋ',	L'ɔ',
L'P',	L'ɒ',	L'R',	L'ʃ',	L'θ',	L'ʊ',	L'ʌ',	L'W',
L'X',	L'Y',	L'ʒ',	L'[',	L'\\',	L']',	L'^',	L'_',
L'`',	L'a',	L'b',	L'c',	L'd',	L'e',	L'f',	L'g',
L'h',	L'i',	L'j',	L'k',	L'l',	L'm',	L'n',	L'o',
L'p',	L'q',	L'r',	L's',	L't',	L'u',	L'v',	L'w',
L'x',	L'y',	L'z',	L'{',	L'|',	L'}',	L'~',	NONE,
};
static Rune grtab[128] = {
NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,
NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,
NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,
NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,
L' ',	L'!',	L'"',	L'#',	L'$',	L'%',	SPCS,	L'\'',
L'(',	L')',	L'*',	L'+',	L',',	L'-',	L'.',	L'/',
L'0',	L'1',	L'2',	L'3',	L'4',	L'5',	L'6',	L'7',
L'8',	L'9',	L':',	L';',	TAGS,	L'=',	TAGE,	L'?',
L'@',	L'Α',	L'Β',	L'Ξ',	L'Δ',	L'Ε',	L'Φ',	L'Γ',
L'Η',	L'Ι',	L'Ϛ',	L'Κ',	L'Λ',	L'Μ',	L'Ν',	L'Ο',
L'Π',	L'Θ',	L'Ρ',	L'Σ',	L'Τ',	L'Υ',	L'V',	L'Ω',
L'Χ',	L'Ψ',	L'Ζ',	L'[',	L'\\',	L']',	L'^',	L'_',
L'`',	L'α',	L'β',	L'ξ',	L'δ',	L'ε',	L'φ',	L'γ',
L'η',	L'ι',	L'ς',	L'κ',	L'λ',	L'μ',	L'ν',	L'ο',
L'π',	L'θ',	L'ρ',	L'σ',	L'τ',	L'υ',	L'v',	L'ω',
L'χ',	L'ψ',	L'ζ',	L'{',	L'|',	L'}',	L'~',	NONE,
};
static Rune subtab[128] = {
NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,
NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,
NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,
NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,
L' ',	L'!',	L'"',	L'#',	L'$',	L'%',	SPCS,	L'\'',
L'₍',	L'₎',	L'*',	L'₊',	L',',	L'₋',	L'.',	L'/',
L'₀',	L'₁',	L'₂',	L'₃',	L'₄',	L'₅',	L'₆',	L'₇',
L'₈',	L'₉',	L':',	L';',	TAGS,	L'₌',	TAGE,	L'?',
L'@',	L'A',	L'B',	L'C',	L'D',	L'E',	L'F',	L'G',
L'H',	L'I',	L'J',	L'K',	L'L',	L'M',	L'N',	L'O',
L'P',	L'Q',	L'R',	L'S',	L'T',	L'U',	L'V',	L'W',
L'X',	L'Y',	L'Z',	L'[',	L'\\',	L']',	L'^',	L'_',
L'`',	L'a',	L'b',	L'c',	L'd',	L'e',	L'f',	L'g',
L'h',	L'i',	L'j',	L'k',	L'l',	L'm',	L'n',	L'o',
L'p',	L'q',	L'r',	L's',	L't',	L'u',	L'v',	L'w',
L'x',	L'y',	L'z',	L'{',	L'|',	L'}',	L'~',	NONE,
};
static Rune suptab[128] = {
NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,
NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,
NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,
NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,	NONE,
L' ',	L'!',	L'"',	L'#',	L'$',	L'%',	SPCS,	L'\'',
L'⁽',	L'⁾',	L'*',	L'⁺',	L',',	L'⁻',	L'.',	L'/',
L'⁰',	L'ⁱ',	L'⁲',	L'⁳',	L'⁴',	L'⁵',	L'⁶',	L'⁷',
L'⁸',	L'⁹',	L':',	L';',	TAGS,	L'⁼',	TAGE,	L'?',
L'@',	L'A',	L'B',	L'C',	L'D',	L'E',	L'F',	L'G',
L'H',	L'I',	L'J',	L'K',	L'L',	L'M',	L'N',	L'O',
L'P',	L'Q',	L'R',	L'S',	L'T',	L'U',	L'V',	L'W',
L'X',	L'Y',	L'Z',	L'[',	L'\\',	L']',	L'^',	L'_',
L'`',	L'a',	L'b',	L'c',	L'd',	L'e',	L'f',	L'g',
L'h',	L'i',	L'j',	L'k',	L'l',	L'm',	L'n',	L'o',
L'p',	L'q',	L'r',	L's',	L't',	L'u',	L'v',	L'w',
L'x',	L'y',	L'z',	L'{',	L'|',	L'}',	L'~',	NONE,
};
static int	tagstarts;
static char	tag[Buflen];
static char	spec[Buflen];
static Entry	curentry;
#define cursize (curentry.end-curentry.start)
static char	*getspec(char *, char *);
static char	*gettag(char *, char *);
void
pgwprintentry(Entry e, int cmd)
{
char *p, *pe;
int t;
long r, rprev, rlig;
Rune *transtab;
p = e.start;
pe = e.end;
transtab = normtab;
rprev = NONE;
changett(0, 0, 0);
curentry = e;
if(cmd == 'h')
outinhibit = 1;
while(p < pe) {
if(cmd == 'r') {
outchar(*p++);
continue;
}
r = transtab[(*p++)&0x7F];
if(r < NONE) {
if(rprev != NONE)
outrune(rprev);
rprev = r;
} else if(r == SPCS) {
p = getspec(p, pe);
r = lookassoc(spectab, asize(spectab), spec);
if(r == -1) {
if(debug)
err("spec %ld %d %s",
e.doff, cursize, spec);
r = L'�';
}
if(r >= LIGS && r < LIGE) {
rlig = liglookup(r, rprev);
if(rlig != NONE)
rprev = rlig;
else {
if(rprev != NONE) outrune(rprev);
rprev = NONE;
}
} else if(r >= MULTI && r < MULTIE) {
if(rprev != NONE) {
outrune(rprev);
rprev = NONE;
}
outrunes(multitab[r-MULTI]);
} else if(r == PAR) {
if(rprev != NONE) {
outrune(rprev);
rprev = NONE;
}
outnl(1);
} else {
if(rprev != NONE) outrune(rprev);
rprev = r;
}
} else if(r == TAGS) {
if(rprev != NONE) {
outrune(rprev);
rprev = NONE;
}
p = gettag(p, pe);
t = lookassoc(tagtab, asize(tagtab), tag);
if(t == -1) {
if(debug)
err("tag %ld %d %s",
e.doff, cursize, tag);
continue;
}
switch(t){
case Hw:
if(cmd == 'h') {
if(!tagstarts)
outchar(' ');
outinhibit = !tagstarts;
}
break;
case Sn:
if(tagstarts) {
outnl(2);
}
break;
case P:
outnl(tagstarts);
break;
case Col:
case Br:
case Blockquote:
if(tagstarts)
outnl(1);
break;
case U:
outchar('/');
}
}
}
if(cmd == 'h') {
outinhibit = 0;
outnl(0);
}
}
long
pgwnextoff(long fromoff)
{
long a, n;
int c;
a = Bseek(bdict, fromoff, 0);
if(a != fromoff)
return -1;
n = 0;
for(;;) {
c = Bgetc(bdict);
if(c < 0)
break;
if(c == '<' && Bgetc(bdict) == 'p' && Bgetc(bdict) == '>') {
c = Bgetc(bdict);
if(c == '<') {
if (Bgetc(bdict) == 'h' && Bgetc(bdict) == 'w'
&& Bgetc(bdict) == '>')
n = 7;
}else if (c == '{')
n = 4;
if(n)
break;
}
}
return (Boffset(bdict)-n);
}
static char *prkey =
"KEY TO THE PRONUNCIATION\n"
"\n"
"I. CONSONANTS\n"
"b, d, f, k, l, m, n, p, t, v, z: usual English values\n"
"\n"
"g as in go (gəʊ)\n"
"h  ...  ho! (həʊ)\n"
"r  ...  run (rʌn), terrier (ˈtɛriə(r))\n"
"(r)...  her (hɜː(r))\n"
"s  ...  see (siː), success (səkˈsɜs)\n"
"w  ...  wear (wɛə(r))\n"
"hw ...  when (hwɛn)\n"
"j  ...  yes (jɛs)\n"
"θ  ...  thin (θin), bath (bɑːθ)\n"
"ð  ...  then (ðɛn), bathe (beɪð)\n"
"ʃ  ...  shop (ʃɒp), dish (dɪʃ)\n"
"tʃ ...  chop (tʃɒp), ditch (dɪtʃ)\n"
"ʒ  ...  vision (ˈvɪʒən), déjeuner (deʒøne)\n"
"dʒ ...  judge (dʒʌdʒ)\n"
"ŋ  ...  singing (ˈsɪŋɪŋ), think (θiŋk)\n"
"ŋg ...  finger (ˈfiŋgə(r))\n"
"\n"
"Foreign\n"
"ʎ as in It. seraglio (serˈraʎo)\n"
"ɲ  ...  Fr. cognac (kɔɲak)\n"
"x  ...  Ger. ach (ax), Sc. loch (lɒx)\n"
"ç  ...  Ger. ich (ɪç), Sc. nicht (nɪçt)\n"
"ɣ  ...  North Ger. sagen (ˈzaːɣən)\n"
"c  ...  Afrikaans baardmannetjie (ˈbaːrtmanəci)\n"
"ɥ  ...  Fr. cuisine (kɥizin)\n"
"\n"
"II. VOWELS AND DIPTHONGS\n"
"\n"
"Short\n"
"ɪ as in pit (pɪt), -ness (-nɪs)\n"
"ɛ  ...  pet (pɛt), Fr. sept (sɛt)\n"
"æ  ...  pat (pæt)\n"
"ʌ  ...  putt (pʌt)\n"
"ɒ  ...  pot (pɒt)\n"
"ʊ  ...  put (pʊt)\n"
"ə  ...  another (əˈnʌðə(r))\n"
"(ə)...  beaten (ˈbiːt(ə)n)\n"
"i  ...  Fr. si (si)\n"
"e  ...  Fr. bébé (bebe)\n"
"a  ...  Fr. mari (mari)\n"
"ɑ  ...  Fr. bâtiment (bɑtimã)\n"
"ɔ  ...  Fr. homme (ɔm)\n"
"o  ...  Fr. eau (o)\n"
"ø  ...  Fr. peu (pø)\n"
"œ  ...  Fr. boeuf (bœf), coeur (kœr)\n"
"u  ...  Fr. douce (dus)\n"
"ʏ  ...  Ger. Müller (ˈmʏlər)\n"
"y  ...  Fr. du (dy)\n"
"\n"
"Long\n"
"iː as in bean (biːn)\n"
"ɑː ...  barn (bɑːn)\n"
"ɔː ...  born (bɔːn)\n"
"uː ...  boon (buːn)\n"
"ɜː ...  burn (bɜːn)\n"
"eː ...  Ger. Schnee (ʃneː)\n"
"ɛː ...  Ger. Fähre (ˈfɛːrə)\n"
"aː ...  Ger. Tag (taːk)\n"
"oː ...  Ger. Sohn (zoːn)\n"
"øː ...  Ger. Goethe (gøːtə)\n"
"yː ...  Ger. grün (gryːn)\n"
"\n"
"Nasal\n"
"ɛ˜, æ˜ as in Fr. fin (fɛ˜, fæ˜)\n"
"ã  ...  Fr. franc (frã)\n"
"ɔ˜ ...  Fr. bon (bɔ˜n)\n"
"œ˜ ...  Fr. un (œ˜)\n"
"\n"
"Dipthongs, etc.\n"
"eɪ as in bay (beɪ)\n"
"aɪ ...  buy (baɪ)\n"
"ɔɪ ...  boy (bɔɪ)\n"
"əʊ ...  no (nəʊ)\n"
"aʊ ...  now (naʊ)\n"
"ɪə ...  peer (pɪə(r))\n"
"ɛə ...  pair (pɛə(r))\n"
"ʊə ...  tour (tʊə(r))\n"
"ɔə ...  boar (bɔə(r))\n"
"\n"
"III. STRESS\n"
"\n"
"Main stress: ˈ preceding stressed syllable\n"
"Secondary stress: ˌ preceding stressed syllable\n"
"\n"
"E.g.: pronunciation (prəˌnʌnsɪˈeɪʃ(ə)n)\n";
void
pgwprintkey(void)
{
Bprint(bout, "%s", prkey);
}
static char *
getspec(char *f, char *fe)
{
char *t;
int c, i;
t = spec;
i = sizeof spec;
while(--i > 0) {
c = *f++;
if(c == ';' || f == fe)
break;
*t++ = c;
}
*t = 0;
return f;
}
static char *
gettag(char *f, char *fe)
{
char *t;
int c, i;
t = tag;
c = *f++;
if(c == '/')
tagstarts = 0;
else {
tagstarts = 1;
*t++ = c;
}
i = Buflen;
while(--i > 0) {
c = *f++;
if(c == '>' || f == fe)
break;
*t++ = c;
}
*t = 0;
return f;
}