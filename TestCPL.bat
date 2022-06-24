@ECHO OFF
ECHO Test Copy File List by copying this CopyList project
MD TestDestCopy

SET FromBS=.\obj\release
IF   EXIST .\obj\debug SET FromBS=.\obj\debug

REM GOTO :BadTest
REM GOTO :BadFromIsDir
REM GOTO :BadToIsFile

ECHO ================ Test with CopyFileList.cwproj.FileList.xml =================
ECHO  CopyFileList From="%FromBS%\CopyFileList.cwproj.FileList.xml" TO=.\TestDestCopy /Debug /Bang /SkipCommon
START CopyFileList From="%FromBS%\CopyFileList.cwproj.FileList.xml" TO=.\TestDestCopy /Debug /Bang /SkipCommon

GOTO :EOF

:BadTest
ECHO ==================== Test with TestBAD.xml  ======================
IF NOT EXIST %FromBS%\TestBAD.xml ECHO You must make %FromBS%\TestBAD.xml & Pause
ECHO  CopyFileList From="%FromBS%\TestBAD.xml" TO=.\TestDestCopy /Debug /Bang /SkipCommon
START CopyFileList From="%FromBS%\TestBAD.xml" TO=.\TestDestCopy /Debug /Bang /SkipCommon

REM You need to make TestBAD.xml by copy FileList.xml then edit it to add some errors like bad file names
GOTO :EOF


:BadFromIsDir
ECHO ==================== Test From=Directory  ======================
ECHO  CopyFileList From="%FromBS%" TO=.\TestDestCopy /Debug /Bang /SkipCommon
START CopyFileList From="%FromBS%" TO=.\TestDestCopy /Debug /Bang /SkipCommon
GOTO :EOF


:BadToIsFile
ECHO ==================== Test TO=File  ======================
REM FYI %0 is is BAT file
COPY %0 .\TestDestCopy
ECHO  CopyFileList From="%FromBS%\CopyFileList.cwproj.FileList.xml" TO=%0 /Debug /Bang /SkipCommon
START CopyFileList From="%FromBS%\CopyFileList.cwproj.FileList.xml" TO=%0 /Debug /Bang /SkipCommonn
GOTO :EOF



REM How to Call ===========================================
@REM new /Bang switch

Usage:
CopyFileList  From=%FileList%  To=%Folder%  [Exclude="%CommaDelimWildcards"]  [/SkipCommon] [/Debug]

Example:
CopyFileList From=C:\Example\Obj\Debug\MyProj.cwproj.FileList.xml To=C:\Tmp\Src2 Exclude="ab*.clw,ab*.inc" /SkipCommon

/SkipCommon will add:
*.ICO,*.OBJ,*.RSC,*.LIB,*.BMP,*.JPG,*.GIF,*.EXE,*.LNK,*.MANIFEST,*.RES,*.CUR
