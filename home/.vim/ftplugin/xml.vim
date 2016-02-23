setlocal sw=2 sts=2 wrap showbreak=+ si tw=79
"setlocal omnifunc=xmlcomplete#CompleteTags

map <F10> :w<CR>:!clear;xmllint --valid --noout <C-r>%<CR>
map <F11> :w<CR>:!clear; xsltproc -o chunks/ /usr/share/xml/docbook/stylesheet/nwalsh/html/chunk.xsl <C-r>%<CR> 
map <S-F11> :w<CR>:!clear; xsltproc -o `basename <C-r>% xml`html --stringparam html.stylesheet style.css /usr/share/xml/docbook/stylesheet/nwalsh/html/docbook.xsl <C-r>%<CR> 

" xml header
"
map <buffer> <LocalLeader>xml i<?xml version="1.0" encoding='UTF-8'?><CR>
imap <buffer> <LocalLeader>xml <?xml version="1.0" encoding='UTF-8'?>

" header and setup for a book / article
"
inoremap <buffer> .dt <!DOCTYPE  PUBLIC "-//OASIS//DTD DocBook XML V4.3//EN"<CR><TAB>"file:///usr/share/xml/docbook/schema/dtd/4.3/docbookx.dtd"><ESC>k02whi

inoremap <buffer> <LocalLeader>dtbk <!DOCTYPE book PUBLIC "-//OASIS//DTD DocBook XML V4.3//EN"<CR><TAB>"file:///usr/share/xml/docbook/schema/dtd/4.3/docbookx.dtd"><CR><CR><BS>
inoremap <buffer> <LocalLeader>bk <book><CR><bookinfo><CR><title></title><CR><author><CR><firstname>Brett</firstname><CR><surname>Campbell</surname><CR></author><CR><address><email></email></address><CR><copyright><CR><year></year><CR><holder></holder><CR></copyright><CR><revhistory><CR></revhistory><CR></bookinfo><CR><CR></book><ESC>12k$bba

noremap <buffer> <LocalLeader>dtart i<!DOCTYPE article PUBLIC "-//OASIS//DTD DocBook XML V4.3//EN"<CR><TAB>"file:///usr/share/xml/docbook/schema/dtd/4.3/docbookx.dtd"><CR><CR><BS>
inoremap <buffer> <LocalLeader>dtart <!DOCTYPE article PUBLIC "-//OASIS//DTD DocBook XML V4.3//EN"<CR><TAB>"file:///usr/share/xml/docbook/schema/dtd/4.3/docbookx.dtd"><CR><CR><BS>

imap <buffer> <LocalLeader>eent <!ENTITY foo ""><ESC>hi

map <buffer> ..rh i<revhistory><CR><TAB><revision><CR><TAB><revnumber></revnumber><CR><date><CR><ESC>!!date '+\%d \%B \%Y'<CR>kJxA</date><CR><BS></revision><CR><BS></revhistory><ESC>3k$2ba
imap <buffer> ..rh <revhistory><CR><TAB><revision><CR><TAB><revnumber></revnumber><CR><date><CR><ESC>!!date '+\%d \%B \%Y'<CR>kJxA</date><CR><BS></revision><CR><BS></revhistory><ESC>3k$2ba
map <buffer> <LocalLeader>rh i<revhistory><ESC>
imap <buffer> <LocalLeader>rh <revhistory><ESC>
map <buffer> .rh i</revhistory><ESC>
imap <buffer> .rh </revhistory><ESC>

map <buffer> <LocalLeader>cdata i<![CDATA[<CR>
imap <buffer> <LocalLeader>cdata <![CDATA[<CR>
map <buffer> .cdata i]]><ESC>
imap <buffer> .cdata ]]><ESC>
map <buffer> ..cdata i<![CDATA[<CR>]]><ESC>O
imap <buffer> ..cdata <![CDATA[<CR>]]><ESC>O

" document element for article
"
map <buffer> ..art i<article><CR><TAB><articleinfo><CR><title></title><CR><TAB><author><CR><TAB><firstname>Brett</firstname><CR><surname>Campbell</surname><CR><affiliation><CR><TAB><address><email></email></address><CR><BS></affiliation><CR><BS></author><CR><abstract><CR><TAB><para><CR></para><CR><BS></abstract><CR><BS></articleinfo><CR><CR><BS></article><ESC>14k$2ba
imap <buffer> ..art <article><CR><TAB><articleinfo><CR><title></title><CR><TAB><author><CR><TAB><firstname>Brett</firstname><CR><surname>Campbell</surname><CR><affiliation><CR><TAB><address><email></email></address><CR><BS></affiliation><CR><BS></author><CR><abstract><CR><TAB><para><CR></para><CR><BS></abstract><CR><BS></articleinfo><CR><CR><BS></article><ESC>14k$2ba

imap <buffer> <LocalLeader>art <article><ESC>
map <buffer> <LocalLeader>art i<article><ESC>
imap <buffer> .art </article><ESC>
map <buffer> .art i</article><ESC>

map <buffer> ..ai i<articleinfo><CR><TAB><title></title><CR><BS></articleinfo><ESC>k$2ba
imap <buffer> ..ai <articleinfo><CR><TAB><title></title><CR><BS></articleinfo><ESC>k$2ba
imap <buffer> <LocalLeader>ai <articleinfo><ESC>
map <buffer> <LocalLeader>ai i<articleinfo><ESC>
imap <buffer> .ai </articleinfo><ESC>
map <buffer> .ai i</articleinfo><ESC>

" special tags/characters
"
map <buffer> <LocalLeader>nb i&nbsp;<ESC>
imap <buffer> <LocalLeader>nb &nbsp;
imap <buffer> <LocalLeader>amp &amp;
imap <buffer> <LocalLeader>lt &lt;
imap <buffer> <LocalLeader>gt &gt;
imap <buffer> <LocalLeader>hb &horbar;
imap <buffer> <LocalLeader>hy &hyphen;

map <buffer> <LocalLeader>cb }{wO<BS><!--<ESC>}bo<BS>--><ESC>
imap <buffer> <LocalLeader>cb <ESC>}{wO<BS><!--<ESC>}bo<BS>--><ESC>

map <buffer> ..cmt i<!--  --><ESC>bhi
imap <buffer> ..cmt <!--  --><ESC>bhi
map <buffer> <LocalLeader>cmt i<!--<ESC>
imap <buffer> <LocalLeader>cmt <!--<ESC>
map <buffer> .cmt i--><ESC>
imap <buffer> .cmt --><ESC>

map <buffer> <LocalLeader>rem i<remark><ESC>
imap <buffer> <LocalLeader>rem <remark><ESC>
map <buffer> .rem i</remark><ESC>
imap <buffer> .rem </remark><ESC>
map <buffer> ..rem ebi<remark><ESC>wea</remark><ESC>
imap <buffer> ..rem <remark></remark><ESC>2ba

" lists
"
imap <buffer> ..li <listitem><CR><TAB><para><CR></para><CR><BS></listitem><ESC>2ko<TAB>
imap <buffer> ..l2 <listitem><CR><TAB><para><CR></para><CR><BS></listitem>
imap <buffer> ..il <itemizedlist mark="bullet"><CR><TAB>..l2<CR><BS></itemizedlist><ESC>2kO<TAB>
imap <buffer> ..ol <orderedlist><CR>..li<CR></orderedlist><ESC>kkk$a
imap <buffer> ..ve <varlistentry><CR><term></term><CR>..l2<CR></varlistentry><ESC>2kO<TAB>
imap <buffer> ..vl <variablelist><CR><TAB><title></title><CR><varlistentry><CR><TAB><term></term><CR>..l2<CR><BS></varlistentry><CR><BS></variablelist>

" sections
"
imap <buffer> ..ss <simplesect id=""><title></title><CR><TAB><para><CR></para><CR><BS></simplesect><ESC>3k$4bla
imap <buffer> ..s1 <sect1 id=""><title></title><CR><TAB><para><CR></para><CR><BS></sect1><ESC>3k$4bla
imap <buffer> ..s2 <sect2 id=""><title></title><CR><TAB><para><CR></para><CR><BS></sect2><ESC>3k$4bla
imap <buffer> ..s3 <sect3 id=""><title></title><CR><TAB><para><CR></para><CR><BS></sect3><ESC>3k$4bla
imap <buffer> ..ch <chapter id=""><title></title><CR><TAB><para><CR></para><CR><BS></chapter><ESC>3k$4bla

imap <buffer> <LocalLeader>ss <simplesect id=""><ESC>hi
map <buffer> <LocalLeader>ss i<simplesect id=""><ESC>hi
imap <buffer> <LocalLeader>s1 <sect1 id=""><ESC>hi
map <buffer> <LocalLeader>s1 i<sect1 id=""><ESC>hi
imap <buffer> <LocalLeader>s2 <sect2 id=""><ESC>hi
map <buffer> <LocalLeader>s2 i<sect2 id=""><ESC>hi
imap <buffer> <LocalLeader>s3 <sect3 id=""><ESC>hi
map <buffer> <LocalLeader>s3 i<sect3 id=""><ESC>hi
imap <buffer> <LocalLeader>ch <chapter id=""><ESC>hi
map <buffer> <LocalLeader>ch i<chapter id=""><ESC>hi

imap <buffer> .ss </simplesect><ESC>
map <buffer> .ss i</simplesect><ESC>
imap <buffer> .s1 </sect1><ESC>
map <buffer> .s1 i</sect1><ESC>
imap <buffer> .s2 </sect2><ESC>
map <buffer> .s2 i</sect2><ESC>
imap <buffer> .s3 </sect3><ESC>
map <buffer> .s3 i</sect3><ESC>
imap <buffer> .ch </chapter><ESC>
map <buffer> .ch i</chapter><ESC>

imap <buffer> ..br <bridgehead id="" renderas="sect2"></bridgehead><ESC>6bla
imap <buffer> <LocalLeader>br <bridgehead id="" renderas="sect2"><ESC>5bla
map <buffer> <LocalLeader>br i<bridgehead id="" renderas="sect2"><ESC>5bla
imap <buffer> .br </bridgehead><ESC>
map <buffer> .br i</bridgehead><ESC>

" computer input/output
"
imap <buffer> ..out <literallayout><computeroutput></computeroutput></literallayout><ESC>4ba
map <buffer> <LocalLeader>out i<computeroutput><ESC>
imap <buffer> <LocalLeader>out <computeroutput><ESC>
map <buffer> .out i</computeroutput><ESC>
imap <buffer> .out </computeroutput><ESC>

map <buffer> ..app ebi<application><ESC>wea</application><ESC>
imap <buffer> ..app <application></application><ESC>2ba

imap <buffer> ..syn <cmdsynopsis><CR><TAB><command></command><CR><arg choice="opt" rep="norepeat"></arg><CR><arg choice="opt" rep="norepeat"></arg><CR><BS></cmdsynopsis><ESC>3k$2ba
map <buffer> <LocalLeader>syn i<cmdsynopsis><ESC>
imap <buffer> <LocalLeader>syn <cmdsynopsis><ESC>
map <buffer> .syn i</cmdsynopsis><ESC>
imap <buffer> .syn </cmdsynopsis><ESC>

map <buffer> ..acr ebi<acronym><ESC>wea</acronym><ESC>
imap <buffer> ..acr <acronym></acronym><ESC>2ba

map <buffer> ..prm ebi<prompt><ESC>wea</prompt><ESC>
imap <buffer> ..prm <prompt></prompt><ESC>2ba
map <buffer> <LocalLeader>prm i<prompt><ESC>
imap <buffer> <LocalLeader>prm <prompt><ESC>
map <buffer> .prm i</prompt><ESC>
imap <buffer> .prm </prompt><ESC>

map <buffer> ..pl ebi<programlisting><ESC>wea</programlisting><ESC>
imap <buffer> ..pl <programlisting></programlisting><ESC>2ba
map <buffer> <LocalLeader>pl i<programlisting><ESC>
imap <buffer> <LocalLeader>pl <programlisting><ESC>
map <buffer> .pl i</programlisting><ESC>
imap <buffer> .pl </programlisting><ESC>

map <buffer> ..ui ebi<userinput><ESC>wea</userinput><ESC>
imap <buffer> ..ui <userinput></userinput><ESC>2ba
map <buffer> <LocalLeader>ui i<userinput><ESC>
imap <buffer> <LocalLeader>ui <userinput><ESC>
map <buffer> .ui i</userinput><ESC>
imap <buffer> .ui </userinput><ESC>

map <buffer> ..cmd ebi<command><ESC>wea</command><ESC>
imap <buffer> ..cmd <command></command><ESC>2ba
map <buffer> <LocalLeader>cmd i<command><ESC>
imap <buffer> <LocalLeader>cmd <command><ESC>
map <buffer> .cmd i</command><ESC>
imap <buffer> .cmd </command><ESC>

map <buffer> ..arg ebi<arg choice="opt" rep="norepeat"><ESC>wea</arg><ESC>
imap <buffer> ..arg <arg choice="opt" rep="norepeat"></arg><ESC>2bla
map <buffer> <LocalLeader>arg i<arg choice="opt" rep="norepeat"><ESC>
imap <buffer> <LocalLeader>arg <arg choice="opt" rep="norepeat"><ESC>
map <buffer> .arg i</arg><ESC>
imap <buffer> .arg <arg><ESC>

map <buffer> ..opt ebi<option><ESC>wea</option><ESC>
imap <buffer> ..opt <option></option><ESC>2ba
imap <buffer> <LocalLeader>opt <option><ESC>
map <buffer> <LocalLeader>opt i<option><ESC>
imap <buffer> .opt </option><ESC>
map <buffer> .opt i</option><ESC>

map <buffer> ..opl ebi<optional><ESC>wea</optional><ESC>
imap <buffer> ..opl <optional></optional><ESC>2ba
imap <buffer> <LocalLeader>opl <optional><ESC>
map <buffer> <LocalLeader>opl i<optional><ESC>
imap <buffer> .opl </optional><ESC>
map <buffer> .opl i</optional><ESC>

map <buffer> ..rep ebi<replaceable><ESC>wea</replaceable><ESC>
imap <buffer> ..rep <replaceable></replaceable><ESC>2ba
imap <buffer> <LocalLeader>rep <replaceable><ESC>
map <buffer> <LocalLeader>rep i<replaceable><ESC>
imap <buffer> .rep </replaceable><ESC>
map <buffer> .rep i</replaceable><ESC>

map <buffer> ..sym ebi<symbol><ESC>wea</symbol><ESC>
imap <buffer> ..sym <symbol></symbol><ESC>2ba
imap <buffer> <LocalLeader>sym <symbol><ESC>
map <buffer> <LocalLeader>sym i<symbol><ESC>
imap <buffer> .sym </symbol><ESC>
map <buffer> .sym i</symbol><ESC>

map <buffer> ..grp i<group choice="opt" rep="norepeat"><CR><TAB><arg choice="plain"></arg><CR><arg choice="plain"></arg><CR><BS></group><ESC>2k$2bla
imap <buffer> ..grp <group choice="opt" rep="norepeat"><CR><TAB><arg choice="plain"></arg><CR><arg choice="plain"></arg><CR><BS></group><ESC>2k$2bla
map <buffer> <LocalLeader>grp i<group choice="opt" rep="norepeat"><ESC>
imap <buffer> <LocalLeader>grp <group choice="opt" rep="norepeat"><ESC>
map <buffer> .grp i</group><ESC>
imap <buffer> .grp </group><ESC>

map <buffer> ..fn i<filename></filename><ESC>2ba
imap <buffer> ..fn <filename></filename><ESC>2ba
map <buffer> ..fn ebi<filename><ESC>wea</filename><ESC>3b
map <buffer> ..f.n ebhi<filename><ESC>wea</filename><ESC>2b
map <buffer> ..f1n ebi<filename><ESC>2wea</filename><ESC>5b
map <buffer> ..f2n ebi<filename><ESC>5wea</filename><ESC>7b
map <buffer> ..f3n ebi<filename><ESC>7wea</filename><ESC>9b
map <buffer> ..f4n ebi<filename><ESC>9wea</filename><ESC>11b
map <buffer> <LocalLeader>fn i<filename><ESC>
imap <buffer> <LocalLeader>fn <filename><ESC>
map <buffer> .fn i</filename><ESC>
imap <buffer> .fn </filename><ESC>

map <buffer> ..err ebi<errortext><ESC>wea</errortext><ESC>
imap <buffer> ..err <errortext></errortext><ESC>2ba

map <buffer> ..var ebi<varname><ESC>wea</varname><ESC>
imap <buffer> ..var <varname></varname><ESC>2ba

map <buffer> ..env ebi<envar><ESC>wea</envar><ESC>
imap <buffer> ..env <envar></envar><ESC>2ba

map <buffer> ..lit ebi<literal><ESC>wea</literal><ESC>
imap <buffer> ..lit <literal></literal><ESC>2ba

" misc
"
imap <buffer> ..ll <literallayout></literallayout><ESC>2ba
imap <buffer> <LocalLeader>ll <literallayout><ESC>
map <buffer> <LocalLeader>ll i<literallayout><ESC>
imap <buffer> .ll </literallayout><ESC>
map <buffer> .ll i</literallayout><ESC>

map <buffer> ..pp }{wO<BS><para><ESC>}bo<BS></para><ESC>
imap <buffer> ..pp <para><CR></para><ESC>O<TAB>
imap <buffer> <LocalLeader>pp <para><ESC>
map <buffer> <LocalLeader>pp i<para><ESC>
imap <buffer> .pp </para><ESC>
map <buffer> .pp i</para><ESC>

map <buffer> ..emp ebi<emphasis><ESC>wea</emphasis><ESC>
imap <buffer> ..emp <emphasis></emphasis><ESC>2ba
imap <buffer> <LocalLeader>emp <emphasis><ESC>
map <buffer> <LocalLeader>emp i<emphasis><ESC>
imap <buffer> .emp </emphasis><ESC>
map <buffer> .emp i</emphasis><ESC>

map <buffer> ..es ebi<emphasis role="strong"><ESC>wea</emphasis><ESC>
imap <buffer> ..es <emphasis role="strong"></emphasis><ESC>b2hi
imap <buffer> <LocalLeader>es <emphasis role="strong"><ESC>
map <buffer> <LocalLeader>es i<emphasis role="strong"><ESC>

map <buffer> ..eu ebi<emphasis role="underline"><ESC>wea</emphasis><ESC>
imap <buffer> ..eu <emphasis role="underline"></emphasis><ESC>b2hi
imap <buffer> <LocalLeader>eu <emphasis role="underline"><ESC>
map <buffer> <LocalLeader>eu i<emphasis role="underline"><ESC>

map <buffer> ..ema ebi<email><ESC>wea</email><ESC>
imap <buffer> ..ema <email></email><ESC>2ba

map <buffer> <LocalLeader>anc i<anchor id=""/><ESC>2hi
imap <buffer> <LocalLeader>anc <anchor id=""/><ESC>2hi

map <buffer> ..ul ebi<ulink url=""><ESC>wea</ulink><ESC>3b2hi
imap <buffer> ..ul <ulink url=""></ulink><ESC>b2hi
imap <buffer> <LocalLeader>ul <ulink url=""><ESC>hi
map <buffer> <LocalLeader>ul i<ulink url=""><ESC>hi
imap <buffer> .ul </ulink><ESC>
map <buffer> .ul i</ulink><ESC>

map <buffer> ..lk ebi<link linkend=""><ESC>wea</link><ESC>3b2hi
imap <buffer> ..lk <link linkend=""></link><ESC>b2hi
imap <buffer> <LocalLeader>lk <link linkend=""><ESC>hi
map <buffer> <LocalLeader>lk i<link linkend=""><ESC>hi
imap <buffer> .lk </link><ESC>
map <buffer> .lk i</link><ESC>

map <buffer> ..tag ebi<sgmltag class=""><ESC>wea</sgmltag><ESC>
imap <buffer> ..tag <sgmltag class=""></sgmltag><ESC>2bla

map <buffer> ..func ebi<function><ESC>wea</function><ESC>
imap <buffer> ..func <function></function><ESC>2ba
map <buffer> <LocalLeader>func i<function><ESC>
imap <buffer> <LocalLeader>func <function><ESC>
map <buffer> .func i</function><ESC>
imap <buffer> .func </function><ESC>

imap <buffer> ..img <imageobject><CR><imagedata fileref="" format=""><CR></imageobject>
imap <buffer> ..mo <mediaobject><CR>,img<ESC>k$hiEPS<ESC>j$a<CR>,img<ESC>k$hiJPG<ESC>j$a<CR></mediaobject>

map <buffer> ..ti ebi<title><ESC>wea</title><ESC>
imap <buffer> ..ti <title></title><ESC>2ba
imap <buffer> <LocalLeader>ti <title><ESC>
map <buffer> <LocalLeader>ti i<title><ESC>
imap <buffer> .ti </title><ESC>
map <buffer> .ti i</title><ESC>

map <buffer> <LocalLeader>qw ebi<quote><ESC>wea</quote><ESC>
map <buffer> ..qu ebi<quote><ESC>wea</quote><ESC>
imap <buffer> ..qu <quote></quote><ESC>2ba
imap <buffer> <LocalLeader>qu <quote><ESC>
map <buffer> <LocalLeader>qu i<quote><ESC>
imap <buffer> .qu </quote><ESC>
map <buffer> .qu i</quote><ESC>

map <buffer> <LocalLeader>sys i<systemitem class=""><ESC>hi
imap <buffer> <LocalLeader>sys <systemitem class=""><ESC>hi
map <buffer> .sys i</systemitem><ESC>
imap <buffer> .sys </systemitem>
map <buffer> ..sys ebi<systemitem class=""><ESC>wea</systemitem><ESC>4bla
imap <buffer> ..sys <systemitem class=""></systemitem><ESC>2bla

map <buffer> ..tok ebi<token><ESC>wea</token><ESC>
imap <buffer> ..tok <token></token><ESC>2ba
map <buffer> <LocalLeader>tok i<token><ESC>
imap <buffer> <LocalLeader>tok <token><ESC>
map <buffer> .tok i</token><ESC>
imap <buffer> .tok </token><ESC>

map <buffer> ..prp ebi<property><ESC>wea</property><ESC>
imap <buffer> ..prp <property></property><ESC>2ba

imap <buffer> ..no <note><CR><para></para><CR></note>
imap <buffer> ..ft <footnote><para></para></footnote>
imap <buffer> ..sb <sidebar><CR><title></title><CR><para></para><CR></sidebar>
imap <buffer> ..w <warning><CR><para></para><CR></warning>
imap <buffer> ..qt <blockquote><CR><attribution></attribution><CR><literallayout><CR></literallayout><CR></blockquote>
imap <buffer> ..ge <glossentry><glossterm></glossterm><CR><glossdef><CR><para><CR></para><CR></glossdef><CR></glossentry><ESC>kkkkk$bba

map <buffer> ..org ebi<orgname class=""><ESC>wea</orgname><ESC>4bla
imap <buffer> ..org <orgname class=""></orgname><ESC>2bla

map <buffer> ..jt ebi<jobtitle><ESC>wea</jobtitle><ESC>
imap <buffer> ..jt <jobtitle></jobtitle><ESC>2ba

imap <buffer> ..faq <article class=faq><CR><title>Frequently asked questions</title><CR><CR><articleinfo><CR><CR><author><CR><firstname></firstname><CR><surname></surname><CR><affiliation><CR><address><email></email></address></affiliation><CR></author><CR><CR><revhistory><CR></revhistory><CR><CR></articleinfo><CR><abstract><CR><indexterm><CR><primary></primary><CR></indexterm><CR><para><CR><para><CR></abstract><CR><CR><qandaset><CR><qandadiv><CR><title></title><CR><qandaentry><CR><question><CR><para></para><CR></question><CR><answer><CR><para></para><CR></answer><CR></qandaentry><CR><qandadiv><CR><qandaset><CR><CR></article><ESC>16k$bba
imap <buffer> ..qd <qandaset><CR><qandadiv><CR><title></title><CR><qandaentry><CR><question><CR><para></para><CR></question><CR><answer><CR><para></para><CR></answer><CR></qandaentry><CR><qandadiv><ESC>9k$bba
imap <buffer> ..qa <qandaentry><CR><question><CR><para></para><CR></question><CR><answer><CR><para></para><CR></answer><CR></qandaentry><ESC>5k$bba

imap <buffer> ..ex <example id=""><CR><title></title><CR></example><ESC>2k$hi
imap <buffer> <LocalLeader>ex <example id=""><ESC>hi
map <buffer> <LocalLeader>ex i<example id=""><ESC>hi
imap <buffer> .ex </example><ESC>
map <buffer> .ex i</example><ESC>

imap <buffer> ..ife <informalexample id=""><CR></informalexample><ESC>k$hi
imap <buffer> <LocalLeader>ife <informalexample id=""><ESC>hi
map <buffer> <LocalLeader>ife i<informalexample id=""><ESC>hi
imap <buffer> .ife </informalexample><ESC>
map <buffer> .ife i</informalexample><ESC>

imap <buffer> ..dat <date><CR><ESC>!!date<CR>kJxA</date>
imap <buffer> ..d2 <date><CR><ESC>!!date '+\%d \%B \%Y'<CR>kJxA</date><ESC>5b
imap <buffer> ..pub <pubdate><emphasis>Last update:<CR><ESC>!!date '+\%d \%B \%Y'<CR>kJA</emphasis></pubdate><ESC>7b

" Tables
"
map <buffer> ..tab i<table frame='none' pgwide='1'><title></title><CR><TAB><tgroup cols=''><CR><TAB><tbody><CR><TAB><row><CR><TAB><entry></entry><CR><BS></row><CR><BS></tbody><CR><BS></tgroup><CR><BS></table><ESC>8k$2ba
imap <buffer> ..tab <table frame='none' pgwide='1'><title></title><CR><TAB><tgroup cols=''><CR><TAB><tbody><CR><TAB><row><CR><TAB><entry></entry><CR><BS></row><CR><BS></tbody><CR><BS></tgroup><CR><BS></table><ESC>8k$2ba
imap <buffer> <LocalLeader>tab <table frame='none' pgwide='1'><ESC>
map <buffer> <LocalLeader>tab i<table frame='none' pgwide='1'><ESC>
imap <buffer> .tab </table><ESC>
map <buffer> .tab i</table><ESC>

map <buffer> ..ift i<informaltable frame='none' pgwide='1'><CR><TAB><tgroup cols=''><CR><TAB><tbody><CR><TAB><row><CR><TAB><entry></entry><CR><BS></row><CR><BS></tbody><CR><BS></tgroup><CR><BS></informaltable><ESC>7k$hi
imap <buffer> ..ift <informaltable frame='none' pgwide='1'><CR><TAB><tgroup cols=''><CR><TAB><tbody><CR><TAB><row><CR><TAB><entry></entry><CR><BS></row><CR><BS></tbody><CR><BS></tgroup><CR><BS></informaltable><ESC>7k$hi
imap <buffer> <LocalLeader>ift <informaltable frame='none' pgwide='1'><ESC>
map <buffer> <LocalLeader>ift i<informaltable frame='none' pgwide='1'><ESC>
imap <buffer> .ift </informaltable><ESC>
map <buffer> .ift i</informaltable><ESC>

imap <buffer> <LocalLeader>tg <tgroup cols=''><ESC>hi
map <buffer> <LocalLeader>tg i<tgroup cols=''><ESC>hi
imap <buffer> .tg </tgroup><ESC>
map <buffer> .tg i</tgroup><ESC>
imap <buffer> ..tg <tgroup cols=''><CR></tgroup><ESC>k$hi

imap <buffer> <LocalLeader>th <thead><ESC>
map <buffer> <LocalLeader>th i<thead><ESC>
imap <buffer> .th </thead><ESC>
map <buffer> .th i</thead><ESC>
imap <buffer> ..th <thead></thead><ESC>2ba

imap <buffer> <LocalLeader>tb <tbody><ESC>
map <buffer> <LocalLeader>tb i<tbody><ESC>
imap <buffer> .tb </tbody><ESC>
map <buffer> .tb i</tbody><ESC>
imap <buffer> ..tb <tbody></tbody><ESC>2ba

imap <buffer> <LocalLeader>row <row><ESC>
map <buffer> <LocalLeader>row i<row><ESC>
imap <buffer> .row </row><ESC>
map <buffer> .row i</row><ESC>
imap <buffer> ..row <row></row><ESC>2ba

imap <buffer> <LocalLeader>ent <entry><ESC>
map <buffer> <LocalLeader>ent i<entry><ESC>
imap <buffer> .ent </entry><ESC>
map <buffer> .ent i</entry><ESC>
imap <buffer> ..ent <entry></entry><ESC>2ba

imap <buffer> <LocalLeader>col <colspec/><ESC>hi
map <buffer> <LocalLeader>col i<colspec/><ESC>hi
imap <buffer> .col <colspec/><ESC>hi
map <buffer> .col i<colspec/><ESC>hi

" EBNF
"
imap <buffer> ..ps <productionset><CR><TAB><production id=""><CR><TAB><lhs></lhs><CR><rhs></rhs><CR><BS></production><CR><BS></productionset><ESC>4k$hi
imap <buffer> <LocalLeader>ps <productionset><ESC>
map <buffer> <LocalLeader>ps i<productionset><ESC>
imap <buffer> .ps </productionset><ESC>
map <buffer> .ps i</productionset><ESC>

imap <buffer> ..prod <production id=""><CR></production><ESC>k$hi
imap <buffer> <LocalLeader>prod <production id=""><ESC>hi
map <buffer> <LocalLeader>prod i<production id=""><ESC>hi
imap <buffer> .prod </production><ESC>
map <buffer> .prod i</production><ESC>

map <buffer> ..lh ebi<lhs><ESC>wea</lhs><ESC>
imap <buffer> ..lh <lhs></lhs><ESC>2ba
map <buffer> ..rh ebi<rhs><ESC>wea</rhs><ESC>
imap <buffer> ..rh <rhs></rhs><ESC>2ba

imap <buffer> <LocalLeader>lh <lhs><ESC>
map <buffer> <LocalLeader>lh i<lhs><ESC>
imap <buffer> .lh </lhs><ESC>
map <buffer> .lh i</lhs><ESC>
imap <buffer> <LocalLeader>rh <rhs><ESC>
map <buffer> <LocalLeader>rh i<rhs><ESC>
imap <buffer> .rh </rhs><ESC>
map <buffer> .rh i</rhs><ESC>

imap <buffer> ..nt <nonterminal def=""></nonterminal><ESC>2bla
imap <buffer> <LocalLeader>nt <nonterminal def=""><ESC>hi
map <buffer> <LocalLeader>nt i<nonterminal def=""><ESC>hi
imap <buffer> .nt </nonterminal><ESC>
map <buffer> .nt i</nonterminal><ESC>

imap <buffer> <LocalLeader>sbr <sbr/>

"imap <buffer> <LocalLeader>con <constraint></constraint><ESC>

" appendix setup
"
map <buffer> ..apdx i<appendix label="" id=""><title></title><CR><TAB><para><CR></para><CR><BS></appendix><ESC>3k$4bla
imap <buffer> ..apdx <appendix label="" id=""><title></title><CR><TAB><para><CR></para><CR><BS></appendix><ESC>3k$4bla
