namespace XPlatLongPaths.IO

type SystemIOFile           = System.IO.File
type SystemIODirectory      = System.IO.Directory
type SystemIOFileSystemInfo = System.IO.FileSystemInfo
type SystemIODirectoryInfo  = System.IO.DirectoryInfo
type SystemIOFileInfo       = System.IO.FileInfo

type OtherFile              = Alphaleonis.Win32.Filesystem.File
type OtherDirectory         = Alphaleonis.Win32.Filesystem.Directory
type OtherFileSystemInfo    = Alphaleonis.Win32.Filesystem.FileSystemInfo
type OtherDirectoryInfo     = Alphaleonis.Win32.Filesystem.DirectoryInfo
type OtherFileInfo          = Alphaleonis.Win32.Filesystem.FileInfo

module internal Internals =
  let runningOnWindows = System.Environment.OSVersion.Platform = System.PlatformID.Win32NT

open Internals

type FileSystemInfo internal (systemIO: Lazy<SystemIOFileSystemInfo>, other: Lazy<OtherFileSystemInfo>) =
  
  internal new (file: OtherFileSystemInfo)    = FileSystemInfo(lazy null, lazy file)
  internal new (file: SystemIOFileSystemInfo) = FileSystemInfo(lazy file, lazy null)
  
  member x.FullName =
    if runningOnWindows then other.Value.FullName
    else systemIO.Value.FullName

  member x.Exists =
    if runningOnWindows then other.Value.Exists
    else systemIO.Value.Exists

  static member internal MakeFromSystemIO(f:SystemIOFileSystemInfo) =
    match f with
    | :? SystemIOFileInfo      as f -> FileInfo(f)      :> FileSystemInfo
    | :? SystemIODirectoryInfo as d -> DirectoryInfo(d) :> FileSystemInfo

  static member internal MakeFromOther(f:OtherFileSystemInfo) =
    match f with
    | :? OtherFileInfo      as f -> FileInfo(f)      :> FileSystemInfo
    | :? OtherDirectoryInfo as d -> DirectoryInfo(d) :> FileSystemInfo

and FileInfo internal (systemIO: Lazy<SystemIOFileInfo>, other:Lazy<OtherFileInfo>) =
  inherit FileSystemInfo(lazy (systemIO.Value :> _), lazy (other.Value :> _))
  
  internal new (file: OtherFileInfo)    = FileInfo(lazy null, lazy file)
  internal new (file: SystemIOFileInfo) = FileInfo(lazy file, lazy null)
  
  new (fullPath: string) = FileInfo(lazy SystemIOFileInfo(fullPath), lazy OtherFileInfo(fullPath))

  member x.Directory =
    if runningOnWindows then other.Value.Directory |> DirectoryInfo
    else systemIO.Value.Directory |> DirectoryInfo

and DirectoryInfo internal (systemIO : Lazy<SystemIODirectoryInfo>, other: Lazy<OtherDirectoryInfo>) =
  inherit FileSystemInfo(lazy (systemIO.Value :> _), lazy (other.Value :> _))
  
  internal new (directory: OtherDirectoryInfo)    = DirectoryInfo(lazy null, lazy directory)
  internal new (directory: SystemIODirectoryInfo) = DirectoryInfo(lazy directory, lazy null)
  
  new (fullPath:string) = DirectoryInfo(lazy SystemIODirectoryInfo(fullPath), lazy OtherDirectoryInfo(fullPath))
  
  member x.EnumerateFiles () =
     if runningOnWindows then other.Value.EnumerateFiles() |> Seq.map FileInfo
     else systemIO.Value.EnumerateFiles() |> Seq.map FileInfo
  
  member x.EnumerateDirectories () =
     if runningOnWindows then other.Value.EnumerateDirectories() |> Seq.map DirectoryInfo
     else systemIO.Value.EnumerateDirectories() |> Seq.map DirectoryInfo
  
  member x.EnumerateFileSystemInfos () =
     if runningOnWindows then other.Value.EnumerateFileSystemInfos() |> Seq.map (FileSystemInfo.MakeFromOther)
     else systemIO.Value.EnumerateFileSystemInfos() |> Seq.map (FileSystemInfo.MakeFromSystemIO)
     
  member x.Create () =
    if runningOnWindows then other.Value.Create()
    else systemIO.Value.Create()

module File =
  
  let AppendAllLines filename lines =
    (filename, lines)
    |> (if runningOnWindows then OtherFile.AppendAllLines else SystemIOFile.AppendAllLines)
    
