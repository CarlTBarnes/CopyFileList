
!Region Notices and Notes
! ================================================================================
! MIT License
! 
! Copyright (c) 2020 Mark Goldberg
! 
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
! 
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
! 
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
! ===============================================================================================
!
!  Description  : A command line tool, to copy content from a FileList.xml to a folder
!  Created      : March 11, 2020
!  Last Updated : March 11, 2020
!
!  C:\> CopyFileList From=%FileList% To=%Folder% [Exclude="%CommaDelimWildcards"] [/SkipCommon]
!
! ===============================================================================================
!EndRegion Notices and Notes

! Possible Future Enhancements
! /NoMessage - remove calls to MESSAGE
!              failures will be HALT N, which can be detected by ERRORLEVEL in batch files
!
! Allow FROM to contain wildcards e.g. FROM=.\Obj\Debug\*.cwproj.FileList.XML to  do multiple projects
! Files names in XML are ALL UPPER. The COPY restult is UPPER.  Would like Original Case.
!    FileNameOriginalCase() done 6/23/22. You MUST delete exising UPPER files in Dest Folder
! Generate a BAT file with all the Copy code?  Sort files by Name or .EXT or Path, in the FileList they are by PATH
! /Include list to only copy specific file specs,might just want ICO. But could take Dest Folder and Delete unwanted

 PROGRAM
 MAP
   Debug(STRING xMessage)
   ODS  (STRING xMessage)
   MODULE('API')
     OutputDebugString(*CSTRING),RAW,PASCAL,NAME('OutputDebugStringA'),DLL(1)
     GetFileAttributes(*CSTRING FileName),LONG,PASCAL,DLL(1),RAW,NAME('GetFileAttributesA')  !Returns Attribs
     FindFirstFile(*CSTRING lpFileName, LONG WIN32_FIND_DATA),LONG,RAW,PASCAL,DLL(1),NAME('FindFirstFileA')
     FindClose(LONG hFindFile),BOOL,PROC,PASCAL,DLL(1)
   END

   IsExcluded(STRING xFileName),BOOL
   ExistsDirectory(STRING pDirName),BOOL            !True if Directory and Not File
   FileNameOriginalCase(*CSTRING InOutFileName )    !Take UPPER file name and find Original Mixed Case
 END

 INCLUDE('StringTheory.inc'),ONCE
 INCLUDE('BigBangTheory.inc'),ONCE      !https://github.com/CarlTBarnes/StringTheory-LoadFile-Split-Viewer

Bang1       BigBangTheory 
BangDbg     BigBangTheory   !For ShowBang
ShowBang    BOOL(FALSE)     !Use /Bang to see BigBang Debug - Dumb name

ShowODS      BOOL(FALSE)

Common       EQUATE('*.ICO,*.OBJ,*.RSC,*.LIB,*.BMP,*.JPG,*.GIF,*.EXE,*.LNK,*.MANIFEST,*.RES,*.CUR')

FormalUsage  EQUATE('CopyFileList  From=%FileList%  To=%Folder%  [Exclude="%CommaDelimWildcards"]  [/SkipCommon] [/Debug] [/Bang]')
ExampleUsage EQUATE('CopyFileList From=C:\Example\Obj\Debug\MyProj.cwproj.FileList.xml To=C:\Tmp\Src2 Exclude="ab*.clw,ab*.inc" /SkipCommon')
Usage        EQUATE(FormalUsage & '||Example:|' & ExampleUsage & '||/SkipCommon will add:|' & Common)


InFile      ANY          ! Filename of FileList.xml
DestFolder  ANY          ! Target folder for the copy
Exclude     StringTheory ! Split list of WildCards to exclude from the copy
FileList    StringTheory ! LoadFile of the FlieList.xml
DoneListCopy    StringTheory ! List of Files Copied
DoneListSkip    StringTheory ! List of Files Skipped
DoneListErrs    StringTheory ! List of Files Errored

!==========================================================================================
 CODE
 SYSTEM{7A7Dh}=1 !PROP:MsgModeDefault added C10 or 11 so all messages allow copy text
 Bang1.LineViewInConsolas = TRUE
 BangDbg.LineViewInConsolas = TRUE
 ShowBang    = CHOOSE( UPPER(COMMAND('/Bang'))  = 'BANG')  ! controls if BigBang Windows show

 ShowODS     = CHOOSE( UPPER(COMMAND('/Debug')) = 'DEBUG') ! controls if Debug messages are sent to OutputDebugString
 InFile      = COMMAND('From')
 DestFolder  = COMMAND('To')
 
                       ! ODS('ShowODS['& ShowODS &']')
                       Debug('Infile['& Infile &']')
                       Debug('DestFolder['& DestFolder &']')

 IF ~InFile OR ~DestFolder
     MESSAGE('Command line requires /FROM= and /TO= Parameters||Note: /FROM should be a FileList.XML' & |
             '||Usage:|' & Usage &'||Command:|' & COMMAND('') ,'CopyFileList Parameters',,'Close',,MSGMODE:CANCOPY)
     HALT(1)
 END
 
 IF ~EXISTS( InFile )
     MESSAGE('FROM='& InFile &' |FROM File Not found|Note: /FROM should be a FileList.XML||Usage:|' & Usage  ,'CopyFileList did NOT Copy')
     HALT(1)
 ELSIF ExistsDirectory( InFile )
     MESSAGE('FROM='& InFile &' |FROM Cannot be a Directory|Note: /FROM should be a FileList.XML||Usage:|' & Usage  ,'CopyFileList did NOT Copy')
     HALT(1)
 END

 IF ~EXISTS(DestFolder)
     MESSAGE('TO='& DestFolder &' |TO Folder Not found||Usage:|' & Usage,'CopyFileList did NOT Copy')
     HALT(2) 
 ELSIF ~ExistsDirectory( DestFolder )
     MESSAGE('TO='& DestFolder &' |TO Must be a Directory||Usage:|' & Usage,'CopyFileList did NOT Copy')
     HALT(2)
 END 

 IF ~FileList.LoadFile( InFile ) THEN       !06/23/22 Carl added IF Failed and Msg
    Message('FileList LoadFile failed' & |
            '||Error: '& FileList.winErrorCode &' '& FileList.LastError & |
            '||File: ' & InFile,'CopyFileList')
    HALT(1)            
 END
 IF ShowBang THEN BangDbg.ValueView(FileList,'FileList '& InFile).
 
 FileList.Split('<13,10>')
 IF ShowBang THEN BangDbg.LinesViewInList(FileList,'FileList Lines '& InFile).

 IF CLIP(LEFT(FileList.GetLine(2))) <>  '<<Opened_Files>'
    CASE MESSAGE('Expected the 2nd line to be <<Opened_Files>' & |
                 '|It was: ' & FileList.GetLine(2) & |
                 '||File: ' & CLIP(InFile) , |
                 'CopyFileList did NOT Copy',,'Close|View File')
    OF 2 ; Bang1.DoNotShow=False 
           Bang1.LinesViewInList(FileList,'FileList Lines '& InFile) 
    END           
    HALT(3)
 END

 DO SetExclude
 DO CopyNow
 RETURN     ! think HALT 0

!---------------------------------
SetExclude                 ROUTINE 
 Exclude.SetValue( UPPER(COMMAND('Exclude')))
 Exclude.Split(',')

 IF COMMAND('/SkipCommon')
    IF Exclude.Records() = 0 
       Exclude.SetValue( Common )
       Exclude.Split(',')
    ELSE 
       DO SetExclude:MergeCommonExclusions
    END 
 END
 IF ShowBang AND Exclude.Records() THEN 
    BangDbg.LinesViewInList(Exclude,'Exclude List' & CHOOSE(~COMMAND('/SkipCommon'),'',' /SkipCommon') )
 END
                             Debug('Exclude.GetValue()['& Exclude.GetValue() &'] Exclude.Records()['& Exclude.Records() &']')
                             LOOP Tmp# = 1 TO Exclude.Records()
                               Debug('Exclude['& Tmp# &'] ' & Exclude.GetLine(Tmp#) )
                             END 

!---------------------------------
SetExclude:MergeCommonExclusions      ROUTINE 
  DATA
ExclusionsToAdd   ANY
ST:Common         StringTheory
CommonRowNum      LONG,AUTO
ExcludeRowNum     LONG,AUTO
Found             BOOL,AUTO
CurrCommon        ANY
  CODE 
  ST:Common.SetValue( Common )
  ST:Common.Split(',')
 
  LOOP CommonRowNum = 1 TO ST:Common.Records()
     CurrCommon = ST:Common.GetLine( CommonRowNum )

     Found=FALSE
     LOOP ExcludeRowNum = 1 TO Exclude.Records()
        IF Exclude.GetLine( ExcludeRowNum) = CurrCommon
           Found=TRUE
           BREAK
        END 
     END 
     IF ~Found 
        ExclusionsToAdd= ExclusionsToAdd & ',' & CurrCommon   ! the ',' on the first entry is just fine (in fact its required)
     END 
  END  

  Exclude.Append( ExclusionsToAdd )
  Exclude.Split(',')

   
!---------------------------------
CopyNow                    ROUTINE 
 DATA 
RowNum         LONG,AUTO
ShouldCopyFile BOOL
CurrRow        StringTheory
Count:Err      LONG(0)
Count:Success  LONG(0)
Count:Skip     LONG(0)
FileName       CSTRING(261) !was ANY use C so same no trailing spaces
 CODE 

 LOOP RowNum = 3 TO FileList.Records()
   CurrRow.SetValue ( FileList.GetLine( RowNum ) )
   IF CLIP(LEFT(CurrRow.GetValue()))= '<</Opened_Files>'
      BREAK 
   END 

   ! example expected content: <file name="C:\SV\CLARION11.0.13244\LIBSRC\WIN\PROPERTY.CLW" />
   FileName = CLIP(CurrRow.Between('"', '"', , , st:NoCase ))
                                         !Debug('FileName['& FileName &']')
   
   IF FileName=''                   
      !blank
   ELSIF IsExcluded( FileName) 
      Debug('Skipping FileName['& FileName &']')
      Count:Skip += 1
      DoneListSkip.AddLine(FileName)
   ELSE
      FileNameOriginalCase(FileName)  !Chnage all UPPER File Name to Mixed Case on Disk
      COPY( FileName, DestFolder)
      IF ErrorCode() 
         Count:Err += 1
         DoneListErrs.AddLine('Error: ' & ErrorCode() &' - '& FileName &' - '& ERROR())
      ELSE 
         Count:Success += 1
         DoneListCopy.AddLine(FileName)
      END 
      Debug('FileName['& FileName &'] ErrorCode()['& ErrorCode() &']=['& Error() &']')      
   END 
 END

    IF ~DoneListCopy.Records() THEN DoneListCopy.AddLine('(None)'). ; DoneListCopy.Join('<13,10>')
    IF ~DoneListSkip.Records() THEN DoneListSkip.AddLine('(None)'). ; DoneListSkip.Join('<13,10>')
    IF ~DoneListErrs.Records() THEN DoneListErrs.AddLine('(None)'). ; DoneListErrs.Join('<13,10>')
    IF ~Exclude.Records() THEN Exclude.AddLine('(None)'). ; Exclude.Join('<13,10>')

 LOOP 
    Bang1.DoNotShow=False
    CASE MESSAGE(  'Count:Success <9>[ '& Count:Success &' ]' |
                & '|Count:Errors  <9>[ '& Count:Err     &' ]' |
                & '|Count:Skipped <9>[ '& Count:Skip    &' ]' |
                & '||Copy From: '  & CLIP(InFile)     |
                & '|Destination: ' & CLIP(DestFolder) |
                & '||Command: ' & COMMAND('')         |
                ,'Done CopyFileList'                  |
                ,                                     |
                ,'&Close|Explore Dest|View Copys|View Errors|Skipped Files|Skip List')
    OF 1 ; BREAK                
    OF 2 ; RUN('Explorer "' & CLIP(DestFolder) &'"')
    OF 3 ; Bang1.LinesViewInList(DoneListCopy,'DoneListCopy - Files Copied')
    OF 4 ; Bang1.LinesViewInList(DoneListErrs,'DoneListErrs - Error on Copy')
    OF 5 ; Bang1.LinesViewInList(DoneListSkip,'DoneListSkip - Skipped Files')
    OF 6 ; Bang1.LinesViewInList(Exclude,'Exclude')
    END
    CYCLE
 END  

!=========================================
IsExcluded PROCEDURE(STRING xFileName) ! assumes xFileName is upper cased
ExcludeRowNum  LONG,AUTO
RetIncluded    BOOL(TRUE)
  CODE 
   LOOP ExcludeRowNum = 1 TO Exclude.Records()
      ! Tests show that Match:Wild is case sensitive, 
      ! so      I did an UPPER on Exclude above
      ! and I will ASSUME that the xFileName is already upper case
      !   as that's how a FileList.xml appears to be written.
      
                                       ! Debug('Match checking Exclude.GetLine(ExcludeRowNum)['& Exclude.GetLine(ExcludeRowNum) &']')

      IF MATCH( xFileName, Exclude.GetLine( ExcludeRowNum ), Match:Wild)
         RETURN TRUE
      END 
   END 

   RETURN FALSE
!=========================================
Debug PROCEDURE(STRING xMessage)
  CODE 
  IF ShowODS
     ODS(xMessage)
  END 
!=========================================
ODS   PROCEDURE(STRING xMessage)
sz  &CSTRING
  CODE 
  sz &= NEW CSTRING( SIZE(xMessage) + 1)
  sz  = xMessage
  OutputDebugString( sz )
  DISPOSE(sz)
!=========================================
ExistsDirectory PROCEDURE(STRING pDirName)  !,BOOL True if Directory and Not file
cFN CSTRING(261),AUTO
FA  LONG,AUTO
eFILE_ATTRIBUTE_DIRECTORY    EQUATE(10h)        !The handle that identifies a directory.
eINVALID_FILE_ATTRIBUTES     EQUATE(0FFFFFFFFh) !(-1) File or folder does not exist
  CODE
  cFN = CLIP(pDirName)
  FA = GetFileAttributes(cFN)
  RETURN CHOOSE(BAND(FA,eFILE_ATTRIBUTE_DIRECTORY) AND FA <> eINVALID_FILE_ATTRIBUTES)
!=======================================================
FileNameOriginalCase  PROCEDURE(*CSTRING InOutFileName)
!The XML FileList has the Names ALL UPPER. 
!For me the Dest Folder files have ALL UPPER names, 
!Hoping to use this to get the original case
!It works BUT the Old UPPER files in the Dest MUST BE DELETED or the Case is preserved as UPPER

!Code from CwUtil FileExists ... Directory() would work but API should be faster
WIN32_FIND_DATA    GROUP,TYPE
dwFileAttributes     ULONG
ftCreationTime       STRING(2*4) !GROUP(FILETIME). ULONG ULONG
ftLastAccessTime     STRING(2*4) !GROUP(FILETIME).
ftLastWriteTime      STRING(2*4) !GROUP(FILETIME).
nFileSizeHigh        ULONG
nFileSizeLow         ULONG
dwReserved0          ULONG
dwReserved1          ULONG
cFileName            CSTRING( FILE:MaxFilePath )
cAlternateFileName   CSTRING( 14 )
                   END
hFindHandle SIGNED,AUTO 
gFileFind   LIKE(WIN32_FIND_DATA),AUTO
InNamePOS   SHORT,AUTO      !Name Position in InOutFileName after path last \
InNameCstr  &CSTRING        !Ref to Name   in InOutFileName after path last \
  CODE
  CLEAR(gFileFind)
  hFindHandle = FindFirstFile(InOutFileName, ADDRESS(gFileFind))
  IF hFindHandle = -1 THEN RETURN.  ! INVALID_HANDLE_VALUE EQUATE(-1)
  FindClose(hFindHandle)
  
  InNamePOS=INSTRING('\',InOutFileName,-1,LEN(InOutFileName)) + 1   !Search backwards for Last \ file name is +1 after, or 0+1=1
? ASSERT(InNamePOS>=1 AND InNamePOS<=SIZE(InOutFileName))
  InNameCstr &= (ADDRESS(InOutFileName[InNamePOS]))                         !Rely on CString <0> to mark end of Name Slice
! IF UPPER(gFileFind.cFileName) = UPPER(CLIP(SUB(InOutFileName,InNamePOS,260))) THEN
  IF UPPER(gFileFind.cFileName) = UPPER(InNameCstr) THEN                    !Be sure Win32 FFF Name is = In Name
     InOutFileName=SUB(InOutFileName,1,InNamePos-1) & gFileFind.cFileName   !   change to Mixed case FFF Name
  END
  RETURN 