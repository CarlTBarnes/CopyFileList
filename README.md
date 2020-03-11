# CopyFileList
Command line util to copy files listed in FileList.xml 

Usage: 
------
C:\> CopyFileList From=%FileList% To=%Folder% [Exclude="%CommaDelimWildcards"] [/SkipCommon] [/Debug]

Examples: 
---------
```c:\> CopyFileList```

Error, will show a usage window


```c:\> CopyFileList From=C:\Src\MyProj\Obj\Debug\MyProj.cwproj.FileList.XML  To=C:\Src\MyProj\Profile```

will copy all files in the Opened Files section of the FileList.xml


```c:\> CopyFileList From=C:\Src\MyProj\Obj\Debug\MyProj.cwproj.FileList.XML  To=C:\Src\MyProj\Profile /SkipCommon```

will copy all files in the Opened Files section of the FileList.xml, except those with common extensions


```c:\> CopyFileList From=C:\Src\MyProj\Obj\Debug\MyProj.cwproj.FileList.XML  To=C:\Src\MyProj\Profile Exclude="ctQ*.*,AB*.*" /SkipCommon```

will copy all files in the Opened Files section of the FileList.xml, except those with common extensions, and additionally skip any files matching `ctQ*.*` or `AB*.*`

---
/Debug 
will write messages to OutputDebugString (ODS)

My favorite ODS viewer is [DebugViewPP](https://github.com/CobaltFusion/DebugViewPP)
Use DebugViewPP 
