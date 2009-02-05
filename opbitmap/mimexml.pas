unit mimexml;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface


var MimeXMLDat:AnsiString;

implementation

begin

MimeXMLDat:='<?xml version="1.0" encoding="UTF-8"?>'+#13#10+
'<!DOCTYPE mime-info ['+#13#10+
'  <!ELEMENT mime-info (mime-type)+>'+#13#10+
'  <!ATTLIST mime-info xmlns CDATA #FIXED "http://www.freedesktop.org/standards/shared-mime-info">'+#13#10+
''+#13#10+
'  <!ELEMENT mime-type (comment|glob|magic)*>'+#13#10+
'  <!ATTLIST mime-type type CDATA #REQUIRED>'+#13#10+
''+#13#10+
'  <!ELEMENT comment (#PCDATA)>'+#13#10+
'  <!ATTLIST comment xml:lang CDATA #IMPLIED>'+#13#10+
''+#13#10+
'  <!ELEMENT glob EMPTY>'+#13#10+
'  <!ATTLIST glob pattern CDATA #REQUIRED>'+#13#10+
''+#13#10+
'  <!ELEMENT magic (match)+>'+#13#10+
'  <!ATTLIST magic priority CDATA #IMPLIED>'+#13#10+
''+#13#10+
'  <!ELEMENT match (match)*>'+#13#10+
'  <!ATTLIST match offset CDATA #REQUIRED>'+#13#10+
'  <!ATTLIST match type (string|big16|big32|little16|little32|host16|host32|byte) #REQUIRED>'+#13#10+
'  <!ATTLIST match value CDATA #REQUIRED>'+#13#10+
'  <!ATTLIST match mask CDATA #IMPLIED>'+#13#10+
']>'+#13#10+
''+#13#10+
'<!-- '+#13#10+
'The freedesktop.org shared MIME database (this file) was created by merging'+#13#10+
'several existing MIME databases (all released under the GPL).'+#13#10+
''+#13#10+
'It comes with ABSOLUTELY NO WARRANTY, to the extent permitted by law. You may'+#13#10+
'redistribute copies of update-mime-database under the terms of the GNU General'+#13#10+
'Public License. For more information about these matters, see the file named'+#13#10+
'COPYING.'+#13#10+
''+#13#10+
'The latest version is available from:'+#13#10+
''+#13#10+
'	http://www.freedesktop.org/standards/shared-mime-info.html'+#13#10+
''+#13#10+
'To extend this database, users and applications should create additional'+#13#10+
'XML files in the ''packages'' directory and run the update-mime-database'+#13#10+
'command to generate the output files.'+#13#10+
'-->'+#13#10+
''+#13#10+
'<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">'+#13#10+
'  <mime-type type="image/bmp">'+#13#10+
'    <comment>Windows BMP image</comment>'+#13#10+
'    <comment xml:lang="af">vensters bmp beeld</comment>'+#13#10+
'    <comment xml:lang="ar">رسم ويندوز BMP</comment>'+#13#10+
'    <comment xml:lang="az">Windows BMP rəsmi</comment>'+#13#10+
'    <comment xml:lang="be">Малюнак Windows BMP</comment>'+#13#10+
'    <comment xml:lang="bg">Графичен файл на Windows BMP</comment>'+#13#10+
'    <comment xml:lang="br">Skeudenn Windows BMP</comment>'+#13#10+
'    <comment xml:lang="bs">Windows BMP slika</comment>'+#13#10+
'    <comment xml:lang="ca">Imatge BMP Windows</comment>'+#13#10+
'    <comment xml:lang="cs">Obrázek ve formátu Windows BMP</comment>'+#13#10+
'    <comment xml:lang="da">Windows BMP-billede</comment>'+#13#10+
'    <comment xml:lang="de">Windows BMP-Bilder</comment>'+#13#10+
'    <comment xml:lang="el">εικόνα Windows BMP</comment>'+#13#10+
'    <comment xml:lang="eo">Vindoza BMP-grafiko</comment>'+#13#10+
'    <comment xml:lang="es">Imagen BMP de Windows</comment>'+#13#10+
'    <comment xml:lang="et">Windows BMP pildifail</comment>'+#13#10+
'    <comment xml:lang="eu">Windows BMP irudia</comment>'+#13#10+
'    <comment xml:lang="fi">Windows BMP -kuva</comment>'+#13#10+
'    <comment xml:lang="fr">Image bitmap MS-Windows</comment>'+#13#10+
'    <comment xml:lang="gl">Imaxe BMP de Windows</comment>'+#13#10+
'    <comment xml:lang="he">תמונת מפת סיביות של Windows</comment>'+#13#10+
'    <comment xml:lang="hr">Windows BMP slika</comment>'+#13#10+
'    <comment xml:lang="hu">Windows BMP kép</comment>'+#13#10+
'    <comment xml:lang="id">Gambar Windows BMP</comment>'+#13#10+
'    <comment xml:lang="is">Windows BMP-mynd(ir)</comment>'+#13#10+
'    <comment xml:lang="it">Immagine BMP di Windows</comment>'+#13#10+
'    <comment xml:lang="ja">Windows BMPイメージ</comment>'+#13#10+
'    <comment xml:lang="ko">윈도우즈 BMP 그림</comment>'+#13#10+
'    <comment xml:lang="lt">Windows BMP paveikslas</comment>'+#13#10+
'    <comment xml:lang="lv">Windows BMP attēls</comment>'+#13#10+
'    <comment xml:lang="mi">Pikitia Windows BMP</comment>'+#13#10+
'    <comment xml:lang="mk">Windows BMP слика</comment>'+#13#10+
'    <comment xml:lang="mt">Stampa Windows BMP</comment>'+#13#10+
'    <comment xml:lang="nb">Windows BMP-bilde</comment>'+#13#10+
'    <comment xml:lang="nl">Windows BMP-afbeelding</comment>'+#13#10+
'    <comment xml:lang="nn">Windows BMP-bilete</comment>'+#13#10+
'    <comment xml:lang="oc">Image BMP Windows</comment>'+#13#10+
'    <comment xml:lang="pl">Obraz BMP (Windows)</comment>'+#13#10+
'    <comment xml:lang="pt">Imagem BMP</comment>'+#13#10+
'    <comment xml:lang="pt_BR">Imagem BMP</comment>'+#13#10+
'    <comment xml:lang="ro">Imagine Windows BMP</comment>'+#13#10+
'    <comment xml:lang="ru">Графический файл Windows BMP</comment>'+#13#10+
'    <comment xml:lang="sk">Windows BMP obrázok</comment>'+#13#10+
'    <comment xml:lang="sl">Slika BMP za Windows</comment>'+#13#10+
'    <comment xml:lang="sr">Windows BMP slika</comment>'+#13#10+
'    <comment xml:lang="sv">Windows BMP-bild</comment>'+#13#10+
'    <comment xml:lang="ta">Windows BMP ¯Õ</comment>'+#13#10+
'    <comment xml:lang="th">แฟ้มภาพวินโดว์ส BMP</comment>'+#13#10+
'    <comment xml:lang="tr">Windows BMP resmi</comment>'+#13#10+
'    <comment xml:lang="uk">Зображення Windows BMP</comment>'+#13#10+
'    <comment xml:lang="ven">Tshifanyiso tsha dziwindo dza BMP</comment>'+#13#10+
'    <comment xml:lang="vi">A?nh BMP của Windows</comment>'+#13#10+
'    <comment xml:lang="xh">Umfanekiso we Windows BMP</comment>'+#13#10+
'    <comment xml:lang="xx">xx</comment>'+#13#10+
'    <comment xml:lang="zh_CN">Windows BMP 图像</comment>'+#13#10+
'    <comment xml:lang="zh_TW">Windows BMP 影像</comment>'+#13#10+
'    <comment xml:lang="zu">Umfanekiso wamawindi e BMP</comment>'+#13#10+
'    <magic priority="50">'+#13#10+
'      <match type="string" mask="0xffff00000000ffff" value="BMxxxx\000\000" offset="0"/>'+#13#10+
'      <match type="string" value="BM" offset="0">'+#13#10+
'        <match type="byte" value="12" offset="14"/>'+#13#10+
'        <match type="byte" value="64" offset="14"/>'+#13#10+
'        <match type="byte" value="40" offset="14"/>'+#13#10+
'      </match>'+#13#10+
'    </magic>'+#13#10+
'    <glob pattern="*.bmp"/>'+#13#10+
'  </mime-type>'+#13#10+
'  <mime-type type="image/gif">'+#13#10+
'    <comment>GIF image</comment>'+#13#10+
'    <comment xml:lang="af">gif beeld</comment>'+#13#10+
'    <comment xml:lang="ar">رسم GIF</comment>'+#13#10+
'    <comment xml:lang="az">GIF rəsmi</comment>'+#13#10+
'    <comment xml:lang="be">Малюнак GIF</comment>'+#13#10+
'    <comment xml:lang="bg">GIF картинка</comment>'+#13#10+
'    <comment xml:lang="br">Skeudenn GIF</comment>'+#13#10+
'    <comment xml:lang="bs">GIF slika</comment>'+#13#10+
'    <comment xml:lang="ca">Imatge GIF</comment>'+#13#10+
'    <comment xml:lang="cs">Obrázek ve formátu GIF</comment>'+#13#10+
'    <comment xml:lang="da">GIF-billede</comment>'+#13#10+
'    <comment xml:lang="de">GIF-Bilder</comment>'+#13#10+
'    <comment xml:lang="el">εικόνα GIF</comment>'+#13#10+
'    <comment xml:lang="eo">GIF-grafiko</comment>'+#13#10+
'    <comment xml:lang="es">Imagen GIF</comment>'+#13#10+
'    <comment xml:lang="et">GIF pildifail</comment>'+#13#10+
'    <comment xml:lang="eu">GIF irudia</comment>'+#13#10+
'    <comment xml:lang="fi">GIF-kuva</comment>'+#13#10+
'    <comment xml:lang="fr">Image GIF</comment>'+#13#10+
'    <comment xml:lang="gl">Imaxe GIF</comment>'+#13#10+
'    <comment xml:lang="he">תמונת GIF</comment>'+#13#10+
'    <comment xml:lang="hr">GIF slika</comment>'+#13#10+
'    <comment xml:lang="hu">GIF kép</comment>'+#13#10+
'    <comment xml:lang="id">Gambar GIF</comment>'+#13#10+
'    <comment xml:lang="is">GIF-mynd(ir)</comment>'+#13#10+
'    <comment xml:lang="it">Immagine GIF</comment>'+#13#10+
'    <comment xml:lang="ja">GIFイメージ</comment>'+#13#10+
'    <comment xml:lang="ko">GIF 그림</comment>'+#13#10+
'    <comment xml:lang="lt">GIF paveikslas</comment>'+#13#10+
'    <comment xml:lang="lv">GIF attēls</comment>'+#13#10+
'    <comment xml:lang="mi">Pikitia GIF</comment>'+#13#10+
'    <comment xml:lang="mk">GIF слика</comment>'+#13#10+
'    <comment xml:lang="mt">Stampa GIF</comment>'+#13#10+
'    <comment xml:lang="nb">GIF-bilde</comment>'+#13#10+
'    <comment xml:lang="nl">GIF-afbeelding</comment>'+#13#10+
'    <comment xml:lang="nn">GIF-bilete</comment>'+#13#10+
'    <comment xml:lang="oc">Image GIF</comment>'+#13#10+
'    <comment xml:lang="pl">Obraz GIF</comment>'+#13#10+
'    <comment xml:lang="pt">Imagem GIF</comment>'+#13#10+
'    <comment xml:lang="pt_BR">Imagem GIF</comment>'+#13#10+
'    <comment xml:lang="ro">Imagine GIF</comment>'+#13#10+
'    <comment xml:lang="ru">Графический файл gif</comment>'+#13#10+
'    <comment xml:lang="sk">GIF obrázok</comment>'+#13#10+
'    <comment xml:lang="sl">Slika GIF</comment>'+#13#10+
'    <comment xml:lang="sr">GIF slika</comment>'+#13#10+
'    <comment xml:lang="sv">Gif-bild</comment>'+#13#10+
'    <comment xml:lang="ta">GIF ¯Õ</comment>'+#13#10+
'    <comment xml:lang="th">แฟ้มภาพ GIF</comment>'+#13#10+
'    <comment xml:lang="tr">GIF resmi</comment>'+#13#10+
'    <comment xml:lang="uk">GIF-образ</comment>'+#13#10+
'    <comment xml:lang="ven">Tshifanyiso tsha GIF</comment>'+#13#10+
'    <comment xml:lang="vi">A?nh GIF</comment>'+#13#10+
'    <comment xml:lang="wa">Imådje GIF</comment>'+#13#10+
'    <comment xml:lang="xh">Umfanekiso we GIF</comment>'+#13#10+
'    <comment xml:lang="xx">xx</comment>'+#13#10+
'    <comment xml:lang="zh_CN">GIF 图像</comment>'+#13#10+
'    <comment xml:lang="zh_TW">GIF 影像</comment>'+#13#10+
'    <comment xml:lang="zu">Umfanekiso we GIF</comment>'+#13#10+
'    <magic priority="50">'+#13#10+
'      <match type="string" value="GIF" offset="0"/>'+#13#10+
'    </magic>'+#13#10+
'    <glob pattern="*.gif"/>'+#13#10+
'  </mime-type>'+#13#10+
'  <mime-type type="image/sgi">'+#13#10+
'	  <comment>SGI Image</comment>'+#13#10+
'	  <comment xml:lang="de">SGI-Bilder</comment>'+#13#10+
'	  <magic priority="50">'+#13#10+
'		  <match type="string" value="\x01\xda\x01" offset="0"/>'+#13#10+
'		  <match type="string" value="\x01\xda\x02" offset="0"/>'+#13#10+
'   </magic>'+#13#10+
'	  <glob pattern="*.bw"/>'+#13#10+
'	  <glob pattern="*.rgba"/>'+#13#10+
'	  <glob pattern="*.sgi"/>'+#13#10+
'  </mime-type>'+#13#10+
'  <mime-type type="image/jpeg">'+#13#10+
'    <comment>JPEG image</comment>'+#13#10+
'    <comment xml:lang="af">jpeg beeld</comment>'+#13#10+
'    <comment xml:lang="ar">رسم JPEG</comment>'+#13#10+
'    <comment xml:lang="az">JPEG rəsmi</comment>'+#13#10+
'    <comment xml:lang="be">Малюнак JPEG</comment>'+#13#10+
'    <comment xml:lang="bg">Графичен файл на JPEG</comment>'+#13#10+
'    <comment xml:lang="br">Skeudenn JPEG</comment>'+#13#10+
'    <comment xml:lang="bs">JPEG slika</comment>'+#13#10+
'    <comment xml:lang="ca">Imatge JPEG</comment>'+#13#10+
'    <comment xml:lang="cs">Obrázek ve formátu JPEG</comment>'+#13#10+
'    <comment xml:lang="da">JPEG-billede</comment>'+#13#10+
'    <comment xml:lang="de">JPEG-Bilder</comment>'+#13#10+
'    <comment xml:lang="el">εικόνα JPEG</comment>'+#13#10+
'    <comment xml:lang="eo">JPEG-grafiko</comment>'+#13#10+
'    <comment xml:lang="es">Imagen JPEG</comment>'+#13#10+
'    <comment xml:lang="et">JPEG pildifail</comment>'+#13#10+
'    <comment xml:lang="eu">JPEG irudia</comment>'+#13#10+
'    <comment xml:lang="fi">JPEG-kuva</comment>'+#13#10+
'    <comment xml:lang="fr">Image JPEG</comment>'+#13#10+
'    <comment xml:lang="gl">Imaxe JPEG</comment>'+#13#10+
'    <comment xml:lang="he">תמונת JPEG</comment>'+#13#10+
'    <comment xml:lang="hr">JPEG slika</comment>'+#13#10+
'    <comment xml:lang="hu">JPEG kép</comment>'+#13#10+
'    <comment xml:lang="id">Gambar JPEG</comment>'+#13#10+
'    <comment xml:lang="is">JPEG-mynd(ir)</comment>'+#13#10+
'    <comment xml:lang="it">Immagine JPEG</comment>'+#13#10+
'    <comment xml:lang="ja">JPEGイメージ</comment>'+#13#10+
'    <comment xml:lang="ko">JPEG 그림</comment>'+#13#10+
'    <comment xml:lang="lt">JPEG paveikslas</comment>'+#13#10+
'    <comment xml:lang="lv">JPEG attēls</comment>'+#13#10+
'    <comment xml:lang="mi">Pikitia JPEG</comment>'+#13#10+
'    <comment xml:lang="mk">JPEG слика</comment>'+#13#10+
'    <comment xml:lang="mt">Stampa JPEG</comment>'+#13#10+
'    <comment xml:lang="nb">JPEG-bilde</comment>'+#13#10+
'    <comment xml:lang="nl">JPEG-afbeelding</comment>'+#13#10+
'    <comment xml:lang="nn">JPEG-bilete</comment>'+#13#10+
'    <comment xml:lang="oc">Image JPEG</comment>'+#13#10+
'    <comment xml:lang="pl">Obraz JPEG</comment>'+#13#10+
'    <comment xml:lang="pt">Imagem JPEG</comment>'+#13#10+
'    <comment xml:lang="pt_BR">Imagem JPEG</comment>'+#13#10+
'    <comment xml:lang="ro">Imagine JPEG</comment>'+#13#10+
'    <comment xml:lang="ru">Графический файл JPEG</comment>'+#13#10+
'    <comment xml:lang="sk">JPEG obrázok</comment>'+#13#10+
'    <comment xml:lang="sl">Slika JPEG</comment>'+#13#10+
'    <comment xml:lang="sr">JPEG slika</comment>'+#13#10+
'    <comment xml:lang="sv">Jpeg-bild</comment>'+#13#10+
'    <comment xml:lang="ta">JPEG ¯Õ</comment>'+#13#10+
'    <comment xml:lang="th">แฟ้มภาพ JPEG</comment>'+#13#10+
'    <comment xml:lang="tr">JPEG resmi</comment>'+#13#10+
'    <comment xml:lang="uk">JPEG-образ</comment>'+#13#10+
'    <comment xml:lang="ven">Tshifanyiso tsha JPEG</comment>'+#13#10+
'    <comment xml:lang="vi">A?nh JPEG</comment>'+#13#10+
'    <comment xml:lang="wa">Imådje JPEG</comment>'+#13#10+
'    <comment xml:lang="xh">Umfanekiso we JPEG</comment>'+#13#10+
'    <comment xml:lang="xx">xx</comment>'+#13#10+
'    <comment xml:lang="zh_CN">JPEG 图像</comment>'+#13#10+
'    <comment xml:lang="zh_TW">JPEG 影像</comment>'+#13#10+
'    <comment xml:lang="zu">Umfanekiso we JPEG</comment>'+#13#10+
'    <magic priority="50">'+#13#10+
'      <match type="string" value="\377\330\377" offset="0"/>'+#13#10+
'      <match type="big16" value="0xffd8" offset="0"/>'+#13#10+
'    </magic>'+#13#10+
'    <glob pattern="*.jpeg"/>'+#13#10+
'    <glob pattern="*.jpg"/>'+#13#10+
'    <glob pattern="*.jpe"/>'+#13#10+
'  </mime-type>'+#13#10+
'  <mime-type type="image/png">'+#13#10+
'    <comment>PNG image</comment>'+#13#10+
'    <comment xml:lang="af">png beeld</comment>'+#13#10+
'    <comment xml:lang="ar">رسم PNG</comment>'+#13#10+
'    <comment xml:lang="az">PNG rəsmi</comment>'+#13#10+
'    <comment xml:lang="be">Малюнак PNG</comment>'+#13#10+
'    <comment xml:lang="bg">Графичен файл на PNG</comment>'+#13#10+
'    <comment xml:lang="br">Skeudenn PNG</comment>'+#13#10+
'    <comment xml:lang="bs">PNG slika</comment>'+#13#10+
'    <comment xml:lang="ca">Imatge PNG</comment>'+#13#10+
'    <comment xml:lang="cs">Obrázek ve formátu PNG</comment>'+#13#10+
'    <comment xml:lang="da">PNG-billede</comment>'+#13#10+
'    <comment xml:lang="de">PNG-Bilder</comment>'+#13#10+
'    <comment xml:lang="el">εικόνα PNG</comment>'+#13#10+
'    <comment xml:lang="eo">PNG-grafiko</comment>'+#13#10+
'    <comment xml:lang="es">Imagen PNG</comment>'+#13#10+
'    <comment xml:lang="et">PNG pildifail</comment>'+#13#10+
'    <comment xml:lang="eu">PNG irudia</comment>'+#13#10+
'    <comment xml:lang="fi">PNG-kuva</comment>'+#13#10+
'    <comment xml:lang="fr">Image PNG</comment>'+#13#10+
'    <comment xml:lang="gl">Imaxe PNG</comment>'+#13#10+
'    <comment xml:lang="he">תמונת PNG</comment>'+#13#10+
'    <comment xml:lang="hr">PNG slika</comment>'+#13#10+
'    <comment xml:lang="hu">PNG kép</comment>'+#13#10+
'    <comment xml:lang="id">Gambar PNG</comment>'+#13#10+
'    <comment xml:lang="is">PNG-mynd(ir)</comment>'+#13#10+
'    <comment xml:lang="it">Immagine PNG</comment>'+#13#10+
'    <comment xml:lang="ja">PNGイメージ</comment>'+#13#10+
'    <comment xml:lang="ko">PNG 그림</comment>'+#13#10+
'    <comment xml:lang="lt">PNG paveikslas</comment>'+#13#10+
'    <comment xml:lang="lv">PNG attēls</comment>'+#13#10+
'    <comment xml:lang="mi">Pikitia PNG</comment>'+#13#10+
'    <comment xml:lang="mk">PNG слика</comment>'+#13#10+
'    <comment xml:lang="mt">Stampa PNG</comment>'+#13#10+
'    <comment xml:lang="nb">PNG-bilde</comment>'+#13#10+
'    <comment xml:lang="nl">PNG-afbeelding</comment>'+#13#10+
'    <comment xml:lang="nn">PNG-bilete</comment>'+#13#10+
'    <comment xml:lang="oc">Image PNG</comment>'+#13#10+
'    <comment xml:lang="pl">Obraz PNG</comment>'+#13#10+
'    <comment xml:lang="pt">Imagem PNG</comment>'+#13#10+
'    <comment xml:lang="pt_BR">Imagem PNG</comment>'+#13#10+
'    <comment xml:lang="ro">Imagine PNG</comment>'+#13#10+
'    <comment xml:lang="ru">Графический файл PNG</comment>'+#13#10+
'    <comment xml:lang="sk">PNG obrázok</comment>'+#13#10+
'    <comment xml:lang="sl">Slika PNG</comment>'+#13#10+
'    <comment xml:lang="sr">PNG slika</comment>'+#13#10+
'    <comment xml:lang="sv">PNG-bild</comment>'+#13#10+
'    <comment xml:lang="ta">PNG ¯Õ</comment>'+#13#10+
'    <comment xml:lang="th">แฟ้มภาพ PNG</comment>'+#13#10+
'    <comment xml:lang="tr">PNG resmi</comment>'+#13#10+
'    <comment xml:lang="uk">PNG-образ</comment>'+#13#10+
'    <comment xml:lang="ven">Tshifanyiso tsha PNG</comment>'+#13#10+
'    <comment xml:lang="vi">A?nh PNG</comment>'+#13#10+
'    <comment xml:lang="wa">Imådje PNG</comment>'+#13#10+
'    <comment xml:lang="xh">Umfanekiso we PNG</comment>'+#13#10+
'    <comment xml:lang="xx">xx</comment>'+#13#10+
'    <comment xml:lang="zh_CN">PNG 图像</comment>'+#13#10+
'    <comment xml:lang="zh_TW">PNG 影像</comment>'+#13#10+
'    <comment xml:lang="zu">Umfanekiso we PNG</comment>'+#13#10+
'    <magic priority="50">'+#13#10+
'      <match type="string" value="\x89PNG" offset="0"/>'+#13#10+
'    </magic>'+#13#10+
'    <glob pattern="*.png"/>'+#13#10+
'  </mime-type>'+#13#10+
'  <mime-type type="image/tiff">'+#13#10+
'    <comment>TIFF Image</comment>'+#13#10+
'    <comment xml:lang="af">tiff beeld</comment>'+#13#10+
'    <comment xml:lang="ar">رسم TIFF</comment>'+#13#10+
'    <comment xml:lang="az">TIFF rəsmi</comment>'+#13#10+
'    <comment xml:lang="be">Малюнак TIFF</comment>'+#13#10+
'    <comment xml:lang="bg">Графичен файл на tiff</comment>'+#13#10+
'    <comment xml:lang="br">Skeudenn TIFF</comment>'+#13#10+
'    <comment xml:lang="bs">TIFF slika</comment>'+#13#10+
'    <comment xml:lang="ca">Imatge TIFF</comment>'+#13#10+
'    <comment xml:lang="cs">Obrázek ve formátu TIFF</comment>'+#13#10+
'    <comment xml:lang="da">TIFF-billede</comment>'+#13#10+
'    <comment xml:lang="de">TIFF-Bilder</comment>'+#13#10+
'    <comment xml:lang="el">εικόνα TIFF</comment>'+#13#10+
'    <comment xml:lang="eo">TIFF-grafiko</comment>'+#13#10+
'    <comment xml:lang="es">Imagen TIFF</comment>'+#13#10+
'    <comment xml:lang="et">TIFF pildifail</comment>'+#13#10+
'    <comment xml:lang="eu">TIFF irudia</comment>'+#13#10+
'    <comment xml:lang="fi">TIFF-kuva</comment>'+#13#10+
'    <comment xml:lang="fr">Image TIFF</comment>'+#13#10+
'    <comment xml:lang="gl">Imaxe TIFF</comment>'+#13#10+
'    <comment xml:lang="he">תמונת TIFF</comment>'+#13#10+
'    <comment xml:lang="hr">TIFF slika</comment>'+#13#10+
'    <comment xml:lang="hu">TIFF kép</comment>'+#13#10+
'    <comment xml:lang="id">Gambar TIFF</comment>'+#13#10+
'    <comment xml:lang="is">TIFF-mynd(ir)</comment>'+#13#10+
'    <comment xml:lang="it">Immagine TIFF</comment>'+#13#10+
'    <comment xml:lang="ja">TIFFイメージ</comment>'+#13#10+
'    <comment xml:lang="ko">TIFF 그림</comment>'+#13#10+
'    <comment xml:lang="lt">TIFF paveikslas</comment>'+#13#10+
'    <comment xml:lang="lv">TIFF Attēls</comment>'+#13#10+
'    <comment xml:lang="mi">Pikitia TIFF</comment>'+#13#10+
'    <comment xml:lang="mk">TIFF слика</comment>'+#13#10+
'    <comment xml:lang="mt">Stampa TIFF</comment>'+#13#10+
'    <comment xml:lang="nb">TIFF-bilde</comment>'+#13#10+
'    <comment xml:lang="nl">TIFF-afbeelding</comment>'+#13#10+
'    <comment xml:lang="nn">TIFF-bilete</comment>'+#13#10+
'    <comment xml:lang="oc">Image TIFF</comment>'+#13#10+
'    <comment xml:lang="pl">Obraz TIFF</comment>'+#13#10+
'    <comment xml:lang="pt">Imagem TIFF</comment>'+#13#10+
'    <comment xml:lang="pt_BR">Imagem TIFF</comment>'+#13#10+
'    <comment xml:lang="ro">Imagine TIFF</comment>'+#13#10+
'    <comment xml:lang="ru">Графический файл tiff</comment>'+#13#10+
'    <comment xml:lang="sk">TIFF obrázok</comment>'+#13#10+
'    <comment xml:lang="sl">Slika TIFF</comment>'+#13#10+
'    <comment xml:lang="sr">TIFF slika</comment>'+#13#10+
'    <comment xml:lang="sv">Tiff-bild</comment>'+#13#10+
'    <comment xml:lang="ta">TIFF ¯Õ</comment>'+#13#10+
'    <comment xml:lang="th">แฟ้มภาพ TIFF</comment>'+#13#10+
'    <comment xml:lang="tr">TIFF resmi</comment>'+#13#10+
'    <comment xml:lang="uk">Зображення TIFF</comment>'+#13#10+
'    <comment xml:lang="ven">Tshifanyiso tsha TIFF</comment>'+#13#10+
'    <comment xml:lang="vi">A?nh TIFF</comment>'+#13#10+
'    <comment xml:lang="wa">Imådje TIFF</comment>'+#13#10+
'    <comment xml:lang="xh">Umfanekiso we TIFF</comment>'+#13#10+
'    <comment xml:lang="xx">xx</comment>'+#13#10+
'    <comment xml:lang="zh_CN">TIFF 图像</comment>'+#13#10+
'    <comment xml:lang="zh_TW">TIFF 影像</comment>'+#13#10+
'    <comment xml:lang="zu">Umfanekiso we TIFF</comment>'+#13#10+
'    <magic priority="50">'+#13#10+
'      <match type="string" value="MM\x00\x2a" offset="0"/>'+#13#10+
'      <match type="string" value="II\x2a\x00" offset="0"/>'+#13#10+
'    </magic>'+#13#10+
'    <glob pattern="*.tif"/>'+#13#10+
'    <glob pattern="*.tiff"/>'+#13#10+
'  </mime-type>'+#13#10+
'  <mime-type type="image/x-pcx">'+#13#10+
'   <comment>PCX image</comment>'+#13#10+
'   <comment xml:lang="bg">Изображение, формат PCX</comment>'+#13#10+
'   <comment xml:lang="cs">Obrázek PCX</comment>'+#13#10+
'   <comment xml:lang="de">PCX-Bild</comment>'+#13#10+
'   <comment xml:lang="es">Imagen PCX</comment>'+#13#10+
'   <comment xml:lang="eu">PCX irudia</comment>'+#13#10+
'   <comment xml:lang="hu">PCX kép</comment>'+#13#10+
'   <comment xml:lang="it">Immagine PCX</comment>'+#13#10+
'   <comment xml:lang="ko">PCX 그림</comment>'+#13#10+
'   <comment xml:lang="nb">PCX-bilde</comment>'+#13#10+
'   <comment xml:lang="nl">PCX-afbeelding</comment>'+#13#10+
'   <comment xml:lang="nn">PCX-bilete</comment>'+#13#10+
'   <comment xml:lang="sv">PCX-bild</comment>'+#13#10+
'   <comment xml:lang="uk">Зображення PCX</comment>'+#13#10+
'   <comment xml:lang="vi">Ảnh PCX</comment>'+#13#10+
'   <magic priority="50">'+#13#10+
'      <match type="string" value="\x0a\x00" offset="0"/>'+#13#10+
'      <match type="string" value="\x0a\x02" offset="0"/>'+#13#10+
'      <match type="string" value="\x0a\x03" offset="0"/>'+#13#10+
'      <match type="string" value="\x0a\x05" offset="0"/>      '+#13#10+
'   </magic>'+#13#10+
'    <glob pattern="*.pcx"/>'+#13#10+
'  </mime-type>'+#13#10+
'  <mime-type type="image/x-photo-cd">'+#13#10+
'    <comment>PhotoCD image</comment>'+#13#10+
'    <comment xml:lang="af">photocd beeld</comment>'+#13#10+
'    <comment xml:lang="ar">رسم PhotoCD</comment>'+#13#10+
'    <comment xml:lang="az">PhotoCD rəsmi</comment>'+#13#10+
'    <comment xml:lang="be">Малюнак PhotoCD</comment>'+#13#10+
'    <comment xml:lang="bg">Графичен файл на PhotoCD</comment>'+#13#10+
'    <comment xml:lang="br">Skeudenn PhotoCD</comment>'+#13#10+
'    <comment xml:lang="bs">PhotoCD slika</comment>'+#13#10+
'    <comment xml:lang="ca">Imatge PhotoCD</comment>'+#13#10+
'    <comment xml:lang="cs">Obrázek ve formátu PhotoCD</comment>'+#13#10+
'    <comment xml:lang="da">PhotoCD-billede</comment>'+#13#10+
'    <comment xml:lang="de">Photo-CD-Bilder</comment>'+#13#10+
'    <comment xml:lang="el">εικόνα PhotoCD</comment>'+#13#10+
'    <comment xml:lang="eo">Fotodisko-grafiko</comment>'+#13#10+
'    <comment xml:lang="es">Imagen PhotoCD</comment>'+#13#10+
'    <comment xml:lang="et">PhotoCD pildifail</comment>'+#13#10+
'    <comment xml:lang="eu">PhotoCD irudia</comment>'+#13#10+
'    <comment xml:lang="fi">PhotoCD-kuva</comment>'+#13#10+
'    <comment xml:lang="fr">Image PhotoCD</comment>'+#13#10+
'    <comment xml:lang="gl">Imaxe PhotoCD</comment>'+#13#10+
'    <comment xml:lang="he">תמונת PhotoCD</comment>'+#13#10+
'    <comment xml:lang="hr">PhotoCD slika</comment>'+#13#10+
'    <comment xml:lang="hu">PhotoCD kép</comment>'+#13#10+
'    <comment xml:lang="id">Gambar PhotoCD</comment>'+#13#10+
'    <comment xml:lang="is">PhotoCD-mynd(ir)</comment>'+#13#10+
'    <comment xml:lang="it">Immagine PhotoCD</comment>'+#13#10+
'    <comment xml:lang="ja">PhotoCDイメージ</comment>'+#13#10+
'    <comment xml:lang="ko">PhotoCD 그림</comment>'+#13#10+
'    <comment xml:lang="lt">PhotoCD paveikslas</comment>'+#13#10+
'    <comment xml:lang="lv">PhotoCD attēls</comment>'+#13#10+
'    <comment xml:lang="mi">Pikitia PhotoCD</comment>'+#13#10+
'    <comment xml:lang="mk">PhotoCD слика</comment>'+#13#10+
'    <comment xml:lang="mt">Stampa PhotoCD</comment>'+#13#10+
'    <comment xml:lang="nb">PhotoCD-bilde</comment>'+#13#10+
'    <comment xml:lang="nl">foto-cd-afbeelding</comment>'+#13#10+
'    <comment xml:lang="nn">PhotoCD-bilete</comment>'+#13#10+
'    <comment xml:lang="oc">Image PhotoCD</comment>'+#13#10+
'    <comment xml:lang="pl">Obraz PhotoCD</comment>'+#13#10+
'    <comment xml:lang="pt">Imagem PhotoCD</comment>'+#13#10+
'    <comment xml:lang="pt_BR">Imagem PhotoCD</comment>'+#13#10+
'    <comment xml:lang="ro">Imagine PhotoCD</comment>'+#13#10+
'    <comment xml:lang="ru">Графический файл PhotoCD</comment>'+#13#10+
'    <comment xml:lang="sk">PhotoCD obrázok</comment>'+#13#10+
'    <comment xml:lang="sl">Slika oblike PhotoCD</comment>'+#13#10+
'    <comment xml:lang="sr">PhotoCD slika</comment>'+#13#10+
'    <comment xml:lang="sv">PhotoCD-bild</comment>'+#13#10+
'    <comment xml:lang="ta">PhotoCD ¯Õ</comment>'+#13#10+
'    <comment xml:lang="th">แฟ้มภาพ PhotoCD</comment>'+#13#10+
'    <comment xml:lang="tr">PhotoCD resmi</comment>'+#13#10+
'    <comment xml:lang="uk">Зображення Photo-CD</comment>'+#13#10+
'    <comment xml:lang="ven">Tshifanyiso tsha tshinepe tsha CD</comment>'+#13#10+
'    <comment xml:lang="vi">A?nh PhotoCD</comment>'+#13#10+
'    <comment xml:lang="xh">Umfanekiso we FotoCD</comment>'+#13#10+
'    <comment xml:lang="xx">xx</comment>'+#13#10+
'    <comment xml:lang="zh_CN">PhotoCD 图像</comment>'+#13#10+
'    <comment xml:lang="zh_TW">PhotoCD 影像</comment>'+#13#10+
'    <comment xml:lang="zu">Umfanekiso we SthombeCD</comment>'+#13#10+
'    <magic priority="50">'+#13#10+
'	    <match type="string" value="PCD" offset="2048"/>'+#13#10+
'	    <match type="string" value="PCD_OPA" offset="0"/>'+#13#10+
'    </magic>'+#13#10+
'    <glob pattern="*.pcd"/>'+#13#10+
'  </mime-type>'+#13#10+
'  <mime-type type="image/x-portable-bitmap">'+#13#10+
'    <comment>Portable Bitmap File Format</comment>'+#13#10+
'    <comment xml:lang="af">draagbaar biskaart lêer formaat</comment>'+#13#10+
'    <comment xml:lang="ar">نسق لمف Portable Bitmap</comment>'+#13#10+
'    <comment xml:lang="bs">Portable Bitmap File format</comment>'+#13#10+
'    <comment xml:lang="cs"> Formát "Portable Bitmap File"</comment>'+#13#10+
'    <comment xml:lang="da">Portable Bitmap filformat</comment>'+#13#10+
'    <comment xml:lang="de">Portierbares Bitmap-Format</comment>'+#13#10+
'    <comment xml:lang="el">Τύπος Αρχείου Portable Bitmap</comment>'+#13#10+
'    <comment xml:lang="eo">Portebla grafiko (PBM)</comment>'+#13#10+
'    <comment xml:lang="es">Formato de archivo Bitmap portable</comment>'+#13#10+
'    <comment xml:lang="et">Portable Bitmap failivorming</comment>'+#13#10+
'    <comment xml:lang="eu">Bitmap Fitxategi Formato Eramangarria</comment>'+#13#10+
'    <comment xml:lang="fi">Portable Bitmap tiedostomuoto</comment>'+#13#10+
'    <comment xml:lang="fr">Format Portable Bitmap</comment>'+#13#10+
'    <comment xml:lang="he">תבנית קובץ מפת סיביות ניידת</comment>'+#13#10+
'    <comment xml:lang="hu">Portable Bitmap fájlformátum</comment>'+#13#10+
'    <comment xml:lang="it">Formato Portable Bitmap</comment>'+#13#10+
'    <comment xml:lang="ja">ポータブルビットマップファイルフォーマット</comment>'+#13#10+
'    <comment xml:lang="lt">Perkeliamas taškinės grafikos (bitmap) bylų formatas</comment>'+#13#10+
'    <comment xml:lang="lv">Pārvietojams Bitmapa Faila Formāts</comment>'+#13#10+
'    <comment xml:lang="nl">overdraagbaar bitmap bestandsformaat</comment>'+#13#10+
'    <comment xml:lang="nn">Portable Bitmap File-format</comment>'+#13#10+
'    <comment xml:lang="pt">Formato Portável de Bitmaps</comment>'+#13#10+
'    <comment xml:lang="pt_BR">Formato de Arquvio Bitmap</comment>'+#13#10+
'    <comment xml:lang="ro">Imagine bitmap în format portabil</comment>'+#13#10+
'    <comment xml:lang="sk"> Formát Portable Bitmap File</comment>'+#13#10+
'    <comment xml:lang="sl">Prenosljiva slikovna datotečna vrsta</comment>'+#13#10+
'    <comment xml:lang="sv">Flyttbart bitmappsfilformat</comment>'+#13#10+
'    <comment xml:lang="th">แฟ้มภาพ Portable Bitmap</comment>'+#13#10+
'    <comment xml:lang="uk">Мобільний формат файлів bitmap</comment>'+#13#10+
'    <comment xml:lang="ven">Muiti wa Bitimepe</comment>'+#13#10+
'    <comment xml:lang="xx">xx</comment>'+#13#10+
'    <comment xml:lang="zh_CN">Portable Bitmap 文件格式</comment>'+#13#10+
'    <comment xml:lang="zu">Ifomathi Yefayela Yephathekayo i Bitmap</comment>'+#13#10+
'    <magic priority="50">'+#13#10+
'      <match type="string" value="P1" offset="0"/>'+#13#10+
'      <match type="string" value="P4" offset="0"/>'+#13#10+
'    </magic>'+#13#10+
'    <glob pattern="*.pbm"/>'+#13#10+
'  </mime-type>'+#13#10+
'  <mime-type type="image/x-portable-graymap">'+#13#10+
'    <comment>Portable Graymap File Format</comment>'+#13#10+
'    <comment xml:lang="ar">نسق لمف Portable Graymap</comment>'+#13#10+
'    <comment xml:lang="bg">Портативен файлов формат за Graymap</comment>'+#13#10+
'    <comment xml:lang="bs">Portable Greymap File format</comment>'+#13#10+
'    <comment xml:lang="cs">Formát "Portable Graymap File"</comment>'+#13#10+
'    <comment xml:lang="da">Portable Graymap Filformat</comment>'+#13#10+
'    <comment xml:lang="de">Portierbares Greymap-Format</comment>'+#13#10+
'    <comment xml:lang="el">Τύπος Αρχείου Portable Graymap</comment>'+#13#10+
'    <comment xml:lang="en_GB">Portable Greymap File Format</comment>'+#13#10+
'    <comment xml:lang="eo">Portebla griza grafiko (PGM)</comment>'+#13#10+
'    <comment xml:lang="es">Formato de archivo portable Graymap</comment>'+#13#10+
'    <comment xml:lang="et">Portable Graymap failivorming</comment>'+#13#10+
'    <comment xml:lang="eu">Graymap Fitxategi Formato Eramangarria</comment>'+#13#10+
'    <comment xml:lang="fi">Portable Graymap tiedostomuoto</comment>'+#13#10+
'    <comment xml:lang="fr">Format Portable Greymap</comment>'+#13#10+
'    <comment xml:lang="he">תבנית קובץ מפת גווני אפור ניידת</comment>'+#13#10+
'    <comment xml:lang="hu">Portable Graymap fájlformátum</comment>'+#13#10+
'    <comment xml:lang="it">Formato Portable Graymap</comment>'+#13#10+
'    <comment xml:lang="ja">ポータブルグレイマップファイルフォーマット</comment>'+#13#10+
'    <comment xml:lang="lt">Perkeliamas graymap bylų formatas</comment>'+#13#10+
'    <comment xml:lang="lv">Pārvietojams Graymap Faila Formāts</comment>'+#13#10+
'    <comment xml:lang="nl">overdraagbaar greymap-bestandsformaat</comment>'+#13#10+
'    <comment xml:lang="nn">Portable Graymap File-format</comment>'+#13#10+
'    <comment xml:lang="pt">Formato Portável de Graymaps</comment>'+#13#10+
'    <comment xml:lang="pt_BR">Formato de Arquivo de Mapas de Escalas Cinza Portável</comment>'+#13#10+
'    <comment xml:lang="ro">Imagine în nuanţe de gri în format portabil</comment>'+#13#10+
'    <comment xml:lang="sk">Formát Portable Graymap File</comment>'+#13#10+
'    <comment xml:lang="sl">Prenosljiva sivinska datotečna vrsta</comment>'+#13#10+
'    <comment xml:lang="sv">Flyttbart gråskalefilformat</comment>'+#13#10+
'    <comment xml:lang="th">แฟ้มภาพ Portable Graymap</comment>'+#13#10+
'    <comment xml:lang="uk">Мобільний формат файлів graymap</comment>'+#13#10+
'    <comment xml:lang="ven">Vhuvha ha faela ya Graymap yau farea</comment>'+#13#10+
'    <comment xml:lang="zh_CN">Portable Graymap 文件格式</comment>'+#13#10+
'    <comment xml:lang="zu">Ifomathi Yefayela Yephathekayo i Graymephu</comment>'+#13#10+
'    <magic priority="50">'+#13#10+
'      <match type="string" value="P2" offset="0"/>'+#13#10+
'      <match type="string" value="P5" offset="0"/>'+#13#10+
'    </magic>'+#13#10+
'    <glob pattern="*.pgm"/>'+#13#10+
'  </mime-type>'+#13#10+
'  <mime-type type="image/x-portable-pixmap">'+#13#10+
'    <comment>Portable Pixmap File Format</comment>'+#13#10+
'    <comment xml:lang="af">draagbaar bispatroon lêer formaat</comment>'+#13#10+
'    <comment xml:lang="ar">نسق لمف Portable Pixmap</comment>'+#13#10+
'    <comment xml:lang="bg">Портативен файлов формат за Pixmap</comment>'+#13#10+
'    <comment xml:lang="bs">Portable Pixmap File format</comment>'+#13#10+
'    <comment xml:lang="cs">Formát "Portable Pixmap File"</comment>'+#13#10+
'    <comment xml:lang="da">Portable Pixmap filformat</comment>'+#13#10+
'    <comment xml:lang="de">Portierbares Pixmap-Format</comment>'+#13#10+
'    <comment xml:lang="el">Τύπος Αρχείου Portable Pixmap</comment>'+#13#10+
'    <comment xml:lang="eo">Portebla grafiko (PPM)</comment>'+#13#10+
'    <comment xml:lang="es">Formato de archivo portable Pixmap</comment>'+#13#10+
'    <comment xml:lang="et">Portable Pixmap failivorming</comment>'+#13#10+
'    <comment xml:lang="eu">Pixmap Fitxategi Formato Eramangarria</comment>'+#13#10+
'    <comment xml:lang="fi">Portable Pixmap tiedostomuoto</comment>'+#13#10+
'    <comment xml:lang="fr">Format Portable Pixmap</comment>'+#13#10+
'    <comment xml:lang="he">תבנית קובץ תמונה ניידת</comment>'+#13#10+
'    <comment xml:lang="hu">Portable Pixmap fájlformátum</comment>'+#13#10+
'    <comment xml:lang="it">Formato Portable Pixmap</comment>'+#13#10+
'    <comment xml:lang="ja">ポータブルピックスマップファイルフォーマット</comment>'+#13#10+
'    <comment xml:lang="lt">Perkeliamas taškinės grafikos (pixmap) bylų formatas</comment>'+#13#10+
'    <comment xml:lang="lv">Pārvietojams Pixmap Faila Formāts</comment>'+#13#10+
'    <comment xml:lang="nl">Overdraagbare pixmap bestandsformaat</comment>'+#13#10+
'    <comment xml:lang="nn">Portable Pixmap File-format</comment>'+#13#10+
'    <comment xml:lang="pt">Formato Portável de Pixmaps</comment>'+#13#10+
'    <comment xml:lang="pt_BR">Formato de Arquivo Pixmap Portável</comment>'+#13#10+
'    <comment xml:lang="ro">Imagine pixmap în format portabil</comment>'+#13#10+
'    <comment xml:lang="sk">Formát Portable Pixmap File</comment>'+#13#10+
'    <comment xml:lang="sl">Prenosljiva slikovna datotečna vrsta</comment>'+#13#10+
'    <comment xml:lang="sv">Flyttbart pixmappfilformat</comment>'+#13#10+
'    <comment xml:lang="th">แฟ้มภาพ Portable Pixmap</comment>'+#13#10+
'    <comment xml:lang="uk">Мобільний формат файлів pixmap</comment>'+#13#10+
'    <comment xml:lang="ven">Vhuvha ha faela ya Pixmap yau farea</comment>'+#13#10+
'    <comment xml:lang="xx">xx</comment>'+#13#10+
'    <comment xml:lang="zh_CN">Portable Pixmap 文件格式</comment>'+#13#10+
'    <comment xml:lang="zu">Ifomathi Yefayela Yephathekayo i Pixmephu</comment>'+#13#10+
'    <magic priority="50">'+#13#10+
'      <match type="string" value="P3" offset="0"/>'+#13#10+
'      <match type="string" value="P6" offset="0"/>'+#13#10+
'    </magic>'+#13#10+
'    <glob pattern="*.ppm"/>'+#13#10+
'  </mime-type>'+#13#10+
'  <mime-type type="image/x-psd">'+#13#10+
'    <comment>Photoshop document</comment>'+#13#10+
'    <comment xml:lang="az">Photoshop sənədi</comment>'+#13#10+
'    <comment xml:lang="ca">document Photoshop</comment>'+#13#10+
'    <comment xml:lang="da">PhotoShop-dokument</comment>'+#13#10+
'    <comment xml:lang="de">Photoshop-Dokument</comment>'+#13#10+
'    <comment xml:lang="el">Έγγραφο Photoshop</comment>'+#13#10+
'    <comment xml:lang="es">documento de Photoshop</comment>'+#13#10+
'    <comment xml:lang="et">Photoshop dokument</comment>'+#13#10+
'    <comment xml:lang="fi">Photoshop-asiakirja</comment>'+#13#10+
'    <comment xml:lang="fr">Document Adobe Photoshop</comment>'+#13#10+
'    <comment xml:lang="gl">Documento do Photoshop</comment>'+#13#10+
'    <comment xml:lang="hu">Photoshop dokumentum</comment>'+#13#10+
'    <comment xml:lang="it">Documento Photoshop</comment>'+#13#10+
'    <comment xml:lang="ja">Photoshop ドキュメント</comment>'+#13#10+
'    <comment xml:lang="ko">포토샵 문서</comment>'+#13#10+
'    <comment xml:lang="lt">Photoshop dokumentas</comment>'+#13#10+
'    <comment xml:lang="ms">Dokumen Photoshop</comment>'+#13#10+
'    <comment xml:lang="nl">Photoshop document</comment>'+#13#10+
'    <comment xml:lang="nn">Photoshop-dokument</comment>'+#13#10+
'    <comment xml:lang="no">Photoshop-dokument</comment>'+#13#10+
'    <comment xml:lang="pl">Dokument Photoshopa</comment>'+#13#10+
'    <comment xml:lang="pt">Documento Photoshop</comment>'+#13#10+
'    <comment xml:lang="ro">Document Photoshop</comment>'+#13#10+
'    <comment xml:lang="ru">Документ формата Photoshop</comment>'+#13#10+
'    <comment xml:lang="sk">dokument Photoshop</comment>'+#13#10+
'    <comment xml:lang="sl">dokument Photoshopa</comment>'+#13#10+
'    <comment xml:lang="sv">Photoshopdokument</comment>'+#13#10+
'    <comment xml:lang="tr">Photoshop belgesi</comment>'+#13#10+
'    <comment xml:lang="uk">документ Photoshop</comment>'+#13#10+
'    <comment xml:lang="wa">documint Photoshop</comment>'+#13#10+
'    <comment xml:lang="zh">Photoshop文档</comment>'+#13#10+
'    <magic priority="50">'+#13#10+
'      <match type="string" mask="0xffffffff0000ffffffff" value="8BPS  \000\000\000\000" offset="0"/>'+#13#10+
'    </magic>'+#13#10+
'    <glob pattern="*.psd"/>'+#13#10+
'  </mime-type>'+#13#10+
'  <mime-type type="image/x-psp">'+#13#10+
'	  <comment>Paint Shop Pro document</comment>'+#13#10+
'	  <comment xml:lang="az">Paint Shop Pro sənədi</comment>'+#13#10+
'	  <comment xml:lang="ca">document Paint Shop Pro</comment>'+#13#10+
'	  <comment xml:lang="da">Paint Shop Pro-dokument</comment>'+#13#10+
'	  <comment xml:lang="de">Paint Shop Pro-Dokument</comment>'+#13#10+
'	  <comment xml:lang="el">Έγγραφο Paint Shop Pro</comment>'+#13#10+
'	  <comment xml:lang="es">documento de Paint Shop Pro</comment>'+#13#10+
'	  <comment xml:lang="et">Paint Shop Pro dokument</comment>'+#13#10+
'	  <comment xml:lang="fi">Paint Shop Pro-asiakirja</comment>'+#13#10+
'	  <comment xml:lang="fr">Document Paint Shop Pro</comment>'+#13#10+
'	  <comment xml:lang="gl">Documento do Paint Shop Pro</comment>'+#13#10+
'	  <comment xml:lang="hu">Paint Shop Pro dokumentum</comment>'+#13#10+
'	  <comment xml:lang="it">Documento Paint Shop Pro</comment>'+#13#10+
'	  <comment xml:lang="ja">Paint Shop Pro ドキュメント</comment>'+#13#10+
'	  <comment xml:lang="ko">포토샵 문서</comment>'+#13#10+
'	  <comment xml:lang="lt">Paint Shop Pro dokumentas</comment>'+#13#10+
'	  <comment xml:lang="ms">Dokumen Paint Shop Pro</comment>'+#13#10+
'	  <comment xml:lang="nl">Paint Shop Pro document</comment>'+#13#10+
'	  <comment xml:lang="nn">Paint Shop Pro-dokument</comment>'+#13#10+
'	  <comment xml:lang="no">Paint Shop Pro-dokument</comment>'+#13#10+
'	  <comment xml:lang="pl">Dokument Paint Shop Pro</comment>'+#13#10+
'	  <comment xml:lang="pt">Documento Paint Shop Pro</comment>'+#13#10+
'	  <comment xml:lang="ro">Document Paint Shop Pro</comment>'+#13#10+
'	  <comment xml:lang="ru">Документ формата Paint Shop Pro</comment>'+#13#10+
'	  <comment xml:lang="sk">dokument Paint Shop Pro</comment>'+#13#10+
'	  <comment xml:lang="sl">dokument Paint Shop Pro</comment>'+#13#10+
'	  <comment xml:lang="sv">Paint Shop Pro dokument</comment>'+#13#10+
'	  <comment xml:lang="tr">Paint Shop Pro belgesi</comment>'+#13#10+
'	  <comment xml:lang="uk">документ Paint Shop Pro</comment>'+#13#10+
'	  <comment xml:lang="wa">documint Paint Shop Pro</comment>'+#13#10+
'	  <comment xml:lang="zh">Paint Shop Pro文档</comment>'+#13#10+
'	  <magic priority="50">'+#13#10+
'		  <match type="string" value="Paint Shop Pro Image File" offset="0"/>'+#13#10+
'	  </magic>'+#13#10+
'	  <glob pattern="*.psp"/>'+#13#10+
'  </mime-type>'+#13#10+
'  <mime-type type="image/x-cut">'+#13#10+
'	  <comment>Dr.Halo document</comment>'+#13#10+
'	  <comment xml:lang="de">Dr. Halo Bild</comment>'+#13#10+
'	  <glob pattern="*.cut"/>'+#13#10+
'  </mime-type>'+#13#10+
'  <mime-type type="image/opb">'+#13#10+
'	  <comment>OPBitmap Image</comment>'+#13#10+
'	  <comment xml:lang="de">OPBitmap Bilder</comment>'+#13#10+
'	  <magic priority="50">'+#13#10+
'		  <match type="string" value="OPB" offset="0"/>'+#13#10+
'	  </magic>'+#13#10+
'	  <glob pattern="*.opb"/>'+#13#10+
'  </mime-type>'+#13#10+
'  <mime-type type="image/x-rgb">'+#13#10+
'    <comment>RGB image</comment>'+#13#10+
'    <comment xml:lang="az">RGB rəsmi</comment>'+#13#10+
'    <comment xml:lang="ca">imatge RGB</comment>'+#13#10+
'    <comment xml:lang="da">RGB-billede</comment>'+#13#10+
'    <comment xml:lang="de">RGB-Bild</comment>'+#13#10+
'    <comment xml:lang="el">Εικόνα RGB</comment>'+#13#10+
'    <comment xml:lang="es">imagen RGB</comment>'+#13#10+
'    <comment xml:lang="et">RGB pilt</comment>'+#13#10+
'    <comment xml:lang="fi">RGB-kuva</comment>'+#13#10+
'    <comment xml:lang="fr">Image RGB</comment>'+#13#10+
'    <comment xml:lang="gl">Imaxe RGB</comment>'+#13#10+
'    <comment xml:lang="hu">RGB kép</comment>'+#13#10+
'    <comment xml:lang="it">Immagine RGB</comment>'+#13#10+
'    <comment xml:lang="ja">RGB 画像</comment>'+#13#10+
'    <comment xml:lang="ko">RGB 이미지</comment>'+#13#10+
'    <comment xml:lang="lt">RGB paveikslėlis</comment>'+#13#10+
'    <comment xml:lang="ms">Imej RGB</comment>'+#13#10+
'    <comment xml:lang="nl">RGB afbeelding</comment>'+#13#10+
'    <comment xml:lang="nn">RGB-bilete</comment>'+#13#10+
'    <comment xml:lang="no">RGB-bilde</comment>'+#13#10+
'    <comment xml:lang="pl">Obraz RGB</comment>'+#13#10+
'    <comment xml:lang="pt">Imagem RGB</comment>'+#13#10+
'    <comment xml:lang="ro">Imagine RGB</comment>'+#13#10+
'    <comment xml:lang="ru">Изображение формата RGB</comment>'+#13#10+
'    <comment xml:lang="sk">obrázok RGB</comment>'+#13#10+
'    <comment xml:lang="sl">slika RGB</comment>'+#13#10+
'    <comment xml:lang="sv">RGB-bild</comment>'+#13#10+
'    <comment xml:lang="tr">RGB görüntüsü</comment>'+#13#10+
'    <comment xml:lang="uk">зображення RGB</comment>'+#13#10+
'    <comment xml:lang="wa">imådje RGB</comment>'+#13#10+
'    <comment xml:lang="zh">RGB图象</comment>'+#13#10+
'	  <magic priority="50">'+#13#10+
'		  <match type="string" value="\x01\xda\x01" offset="0"/>'+#13#10+
'		  <match type="string" value="\x01\xda\x02" offset="0"/>'+#13#10+
'   </magic>'+#13#10+
'    <glob pattern="*.rgb"/>'+#13#10+
'  </mime-type>'+#13#10+
'  <mime-type type="image/x-tga">'+#13#10+
'    <comment>TarGA image</comment>'+#13#10+
'    <comment xml:lang="az">TarGA rəsmi</comment>'+#13#10+
'    <comment xml:lang="ca">imatge TarGA</comment>'+#13#10+
'    <comment xml:lang="da">TarGA-billede</comment>'+#13#10+
'    <comment xml:lang="de">TarGA-Bild</comment>'+#13#10+
'    <comment xml:lang="el">Εικόνα TarGA</comment>'+#13#10+
'    <comment xml:lang="es">imagen TarGA</comment>'+#13#10+
'    <comment xml:lang="et">TarGA pilt</comment>'+#13#10+
'    <comment xml:lang="fi">TarGA-kuva</comment>'+#13#10+
'    <comment xml:lang="fr">Image TarGA</comment>'+#13#10+
'    <comment xml:lang="gl">Imaxe TarGA</comment>'+#13#10+
'    <comment xml:lang="hu">TarGA kép</comment>'+#13#10+
'    <comment xml:lang="it">Immagine TarGA</comment>'+#13#10+
'    <comment xml:lang="ja">TarGA 画像</comment>'+#13#10+
'    <comment xml:lang="ko">TarGA 이미지</comment>'+#13#10+
'    <comment xml:lang="lt">TarGA paveikslėlis</comment>'+#13#10+
'    <comment xml:lang="ms">Imej TarGA</comment>'+#13#10+
'    <comment xml:lang="nl">TarGA afbeelding</comment>'+#13#10+
'    <comment xml:lang="nn">TarGA-bilete</comment>'+#13#10+
'    <comment xml:lang="no">TarGA-bilde</comment>'+#13#10+
'    <comment xml:lang="pl">Obraz TarGA</comment>'+#13#10+
'    <comment xml:lang="pt">Imagem TarGa</comment>'+#13#10+
'    <comment xml:lang="ro">Imagine TarGA</comment>'+#13#10+
'    <comment xml:lang="ru">Изображение формата TarGA</comment>'+#13#10+
'    <comment xml:lang="sk">obrázok TarGA</comment>'+#13#10+
'    <comment xml:lang="sl">slika TarGA</comment>'+#13#10+
'    <comment xml:lang="sv">TarGA-bild</comment>'+#13#10+
'    <comment xml:lang="tr">TarGA görüntüsü</comment>'+#13#10+
'    <comment xml:lang="uk">зображення TarGA</comment>'+#13#10+
'    <comment xml:lang="wa">imådje TarGA</comment>'+#13#10+
'    <comment xml:lang="zh">TarGA图象</comment>'+#13#10+
'    <magic priority="50">'+#13#10+
'	    <match value="0x00000200" type="big32" offset="0"/>'+#13#10+
'    </magic>    '+#13#10+
'    <glob pattern="*.tga"/>'+#13#10+
'  </mime-type>'+#13#10+
'</mime-info>';

end.

