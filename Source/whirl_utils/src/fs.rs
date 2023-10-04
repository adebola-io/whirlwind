// use std::{
//     cell::RefCell,
//     ffi::{OsStr, OsString},
//     io,
//     path::{Iter, Path, PathBuf},
//     rc::{Rc, Weak},
// };

// /// Trait for a bare bones mock file system.
// pub trait FileSystem<ReadDir, DirEntry>
// where
//     ReadDir: Iterator<Item = io::Result<DirEntry>>,
//     DirEntry: ConvertibleToPath,
// {
//     /// Read a file's contents into a string.
//     fn read_to_string<P: AsRef<Path>>(&self, path: P) -> io::Result<String>;
//     /// Check if a path exists.
//     fn exists(&self, path: &Path) -> bool;
//     /// Creates a directory in the file system.
//     fn create_dir<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()>;
//     /// Returns an iterator over the entries within a directory
//     fn read_dir<P: AsRef<Path>>(&self, path: P) -> io::Result<ReadDir>;
//     /// Turns a path into an absolute path.
//     fn canonicalize<P: AsRef<Path>>(&self, path: P) -> io::Result<PathBuf>;
//     /// Write to a file, creating it if it does not exist.
//     fn write<P: AsRef<Path>, C: AsRef<[u8]>>(&mut self, path: P, contents: C) -> io::Result<()>;
//     /// Removes a file from the filesystem.
//     fn remove_file<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()>;
//     /// Removes an empty directory.
//     fn remove_dir<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()>;
//     /// Returns true if an entry is a file.
//     fn is_file(&self, path: &Path) -> bool;
//     /// Returns true if an entry is a directory.
//     fn is_dir(&self, path: &Path) -> bool;
// }

// pub trait ConvertibleToPath {
//     fn path(&self) -> PathBuf;
// }

// /// A file system that only exists in memory.
// /// Useful for mocking file system operations.
// #[derive(Debug)]
// pub struct VirtualFileSystem {
//     root: Rc<RefCell<VirtualDirectory>>,
// }

// /// A virtual "directory".
// #[derive(Debug)]
// pub struct VirtualDirectory {
//     parent: Option<Weak<RefCell<VirtualDirEntry>>>,
//     folder_name: OsString,
//     entries: Vec<Rc<RefCell<VirtualDirEntry>>>,
// }

// #[derive(Debug)]
// pub enum VirtualDirEntry {
//     File(VirtualFile),
//     Folder(VirtualDirectory),
// }

// /// A virtual "file".
// #[derive(Debug)]
// pub struct VirtualFile {
//     parent: Option<Weak<RefCell<VirtualDirEntry>>>,
//     file_name: OsString,
//     contents: String,
// }

// pub struct VirtualReadDir<'a> {
//     start: u32,
//     entries: &'a Vec<Rc<RefCell<VirtualDirEntry>>>,
// }

// pub struct PathFinder<'a> {
//     addresses: Iter<'a>,
//     current: Rc<RefCell<VirtualDirEntry>>,
// }

// /// Wrapper around the actual underlying file system.
// pub struct RealFileSystem {}

// impl VirtualDirEntry {
//     /// Returns the parent folder of this entry.
//     fn parent_folder(&self) -> Option<Rc<RefCell<VirtualDirEntry>>> {
//         match self {
//             VirtualDirEntry::File(file) => Weak::upgrade(&file.parent.clone()?),
//             VirtualDirEntry::Folder(folder) => Weak::upgrade(&folder.parent.clone()?),
//         }
//     }
//     /// Returns the name of the entry.
//     fn name(&self) -> &OsStr {
//         match self {
//             VirtualDirEntry::File(file) => file.file_name.as_os_str(),
//             VirtualDirEntry::Folder(folder) => folder.folder_name.as_os_str(),
//         }
//     }
// }

// impl VirtualFileSystem {
//     /// Creates a new virtual system.
//     pub fn new(root_folder_name: &str) -> Self {
//         let root = Rc::new(RefCell::new(VirtualDirectory {
//             parent: None,
//             folder_name: OsString::from(root_folder_name),
//             entries: vec![],
//         }));
//         VirtualFileSystem { root }
//     }
//     /// Returns the root folder name.
//     pub fn root_dir_name(&self) -> &OsStr {
//         unsafe { &*self.root.as_ptr() }.folder_name.as_os_str()
//     }
// }

// impl<'a> FileSystem<VirtualReadDir<'a>, &'a VirtualDirEntry> for VirtualFileSystem {
//     fn read_to_string<P: AsRef<Path>>(&self, path: P) -> io::Result<String> {
//         todo!()
//     }

//     fn exists(&self, path: &Path) -> bool {
//         todo!()
//     }

//     fn create_dir<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
//         let path = PathBuf::from(path.as_ref());
//         let current = &self.root;
//         let paths = path.into_iter().collect::<Vec<_>>();

//         Ok(())
//     }

//     fn read_dir<P: AsRef<Path>>(&self, path: P) -> io::Result<VirtualReadDir<'a>> {
//         todo!()
//     }

//     fn canonicalize<P: AsRef<Path>>(&self, path: P) -> io::Result<PathBuf> {
//         todo!()
//     }

//     fn write<P: AsRef<Path>, C: AsRef<[u8]>>(&mut self, path: P, contents: C) -> io::Result<()> {
//         todo!()
//     }

//     fn remove_file<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
//         todo!()
//     }

//     fn remove_dir<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
//         todo!()
//     }

//     fn is_file(&self, path: &Path) -> bool {
//         todo!()
//     }

//     fn is_dir(&self, path: &Path) -> bool {
//         todo!()
//     }
// }

// impl<'a> Iterator for VirtualReadDir<'a> {
//     type Item = io::Result<&'a VirtualDirEntry>;

//     fn next(&mut self) -> Option<Self::Item> {
//         unsafe {
//             if (self.start as usize) == self.entries.len() {
//                 return None;
//             }
//             let entry = &*self.entries[self.start as usize].as_ptr();
//             self.start += 1;
//             Some(Ok(entry))
//         }
//     }
// }

// impl ConvertibleToPath for &VirtualDirEntry {
//     fn path(&self) -> PathBuf {
//         todo!()
//     }
// }

// impl PathFinder<'_> {
//     /// Try to follow a path to its destination file or folder.
//     fn find(mut self) -> Option<VirtualDirEntry> {
//         let current = self.current.clone();
//         let current = current.borrow();
//         'address_loop: for path_component in self.addresses {
//             match path_component.to_str()? {
//                 // go up one level.
//                 ".." => self.current = current.parent_folder()?,
//                 "." => {}
//                 address => match &*(current) {
//                     // Cannot nest lower than a file.
//                     VirtualDirEntry::File(_) => return None,
//                     VirtualDirEntry::Folder(current) => {
//                         // Find the correct entry.
//                         for entry in &current.entries {
//                             if entry.borrow().name() == address {
//                                 self.current = entry.clone();
//                                 continue 'address_loop;
//                             }
//                         }
//                         // no match found.
//                         return None;
//                     }
//                 },
//             }
//         }
//         todo!()
//     }
// }

// #[cfg(test)]
// mod tests {
//     use std::path::PathBuf;

//     use crate::{FileSystem, VirtualFileSystem};

//     #[test]
//     fn testing_virtual_fs_creation() {
//         let fs = VirtualFileSystem::new("testing");
//         assert_eq!(fs.root_dir_name(), "testing");
//         println!(
//             "{:?}",
//             PathBuf::from("../../Tests/binding.sd/variables.wrl").canonicalize()
//         )
//     }

//     #[test]
//     fn create_new_folder() {
//         let mut fs = VirtualFileSystem::new("testing");
//         fs.create_dir("/test-folder//.././Assets").unwrap();
//         assert!(fs.exists(&PathBuf::from("/test-folder")));
//     }
// }
