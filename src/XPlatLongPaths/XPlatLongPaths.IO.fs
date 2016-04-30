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
  let inline doEither a b = if runningOnWindows then a () else b ()
  let inline (=>) a f = fun () -> f a // sorry
open Internals

type [<AllowNullLiteral>] FileSystemInfo internal (systemIO: Lazy<SystemIOFileSystemInfo>, other: Lazy<OtherFileSystemInfo>) =
  
  internal new (file: OtherFileSystemInfo)    = FileSystemInfo(lazy null, lazy file)
  internal new (file: SystemIOFileSystemInfo) = FileSystemInfo(lazy file, lazy null)
  
  member x.FullName =
    if runningOnWindows then other.Value.FullName
    else systemIO.Value.FullName

  member x.Name =
    if runningOnWindows then other.Value.Name
    else systemIO.Value.Name

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

and [<AllowNullLiteral>] DirectoryInfo internal (systemIO : Lazy<SystemIODirectoryInfo>, other: Lazy<OtherDirectoryInfo>) =
  inherit FileSystemInfo(lazy (systemIO.Value :> _), lazy (other.Value :> _))
  
  internal new (directory: OtherDirectoryInfo)    = DirectoryInfo(lazy null, lazy directory)
  internal new (directory: SystemIODirectoryInfo) = DirectoryInfo(lazy directory, lazy null)
  
  new (fullPath:string) = DirectoryInfo(lazy SystemIODirectoryInfo(fullPath), lazy OtherDirectoryInfo(fullPath))
  
  member x.EnumerateFiles () =
     if runningOnWindows then other.Value.EnumerateFiles() |> Seq.map FileInfo
     else systemIO.Value.EnumerateFiles() |> Seq.map FileInfo
  
  member x.EnumerateDirectories () =
     doEither
      (other.Value.EnumerateDirectories    >> Seq.map DirectoryInfo)
      (systemIO.Value.EnumerateDirectories >> Seq.map DirectoryInfo)
    
  member x.GetDirectories () =
     doEither
      (other.Value.GetDirectories    >> Array.map DirectoryInfo)
      (systemIO.Value.GetDirectories >> Array.map DirectoryInfo)
  
  member x.EnumerateFileSystemInfos () =
     doEither
      (other.Value.EnumerateFileSystemInfos    >> Seq.map FileSystemInfo.MakeFromOther)
      (systemIO.Value.EnumerateFileSystemInfos >> Seq.map FileSystemInfo.MakeFromSystemIO)
   
  member x.Create () =
    if runningOnWindows then other.Value.Create()
    else systemIO.Value.Create()

  member x.Parent =
    if runningOnWindows then 
      if isNull other.Value.Parent then null
      else (other.Value.Parent |> DirectoryInfo)
    else 
      if isNull systemIO.Value.Parent then null
      else (systemIO.Value.Parent |> DirectoryInfo)

module File =
  
  let AppendAllLines filename lines =
    (filename, lines)
    |> (if runningOnWindows then OtherFile.AppendAllLines else SystemIOFile.AppendAllLines)
  let Exists filename =
    doEither
      (filename => OtherFile.Exists)
      (filename => SystemIOFile.Exists)

module Directory =
  let CreateDirectory name =
      doEither
        (name => OtherDirectory.CreateDirectory >> DirectoryInfo)
        (name => SystemIODirectory.CreateDirectory >> DirectoryInfo)
