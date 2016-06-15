%% dsdfds
-define(NR_FILE,8192).
-define(NR_OPEN,1024*1024).
-define(NR_RESERVED_FILES,10).
-define(NR_SUPER,256).

-define(MAY_EXEC,1).
-define(MAY_WRITE,2).
-define(MAY_READ,4).

-define(FMODE_READ,1).
-define(FMODE_WRITE,2).

-define(READ,0).
-define(WRITE,1).
-define(READA,2).
-define(SPECIAL,4).

%% fs-independent mount flags
-define(MS_READONLY,1).
-define(MS_NOSUID,2).
  -define(MS_NODEV,4).
  -define(MS_NOEXEC,8).
  -define(MS_SYNCHRONOUS,16).
-define(MS_REMOUNT,32).
  -define(MS_MANDLOCK,64).
  -define(MS_NOATIME,1024).
  -define(MS_NODIRATIME,2048).
-define(MS_BIND,4096).
-define(MS_RMT_MASK, ?MS_READONLY bor ?MS_NOSUID bor ?MS_NODEV bor ?MS_NOEXEC
	bor ?MS_SYNCHRONOUS bor ?MS_MANDLOCK bor ?MS_NODIRATIME).


%% i_state flags
-define(I_DIRTY,).
-define(I_LOCK,).
-define(I_FREEING,).

%% i_flags
  -define(S_SYNC,1).
-define(S_NOATIME,2).
  -define(S_QUOTA,4).
  -define(S_APPEND,8).
  -define(S_IMMUTABLE,16).
  -define(S_DEAD,32).

-define (ATTR_FLAG_SYNCHRONOUS,1).
-define (ATTR_FLAG_NOATIME,2).
-define (ATTR_FLAG_APPEND,4).
-define (ATTR_FLAG_IMMUTABLE,8).
-define (ATTR_FLAG_NODIRATIME,16).

-record(inode,
	{
	  i_hash :: list_head(),
	  i_list :: list_head(),
	  i_dentry :: list_head(),
	  i_ino :: non_neg_integer(),
	  i_count :: non_neg_integer(),
	  i_dev :: kdev_t(),
	  i_mode :: umode_t(),
	  i_nlink :: nlink_t(),
	  i_uid :: uid_t(),
	  i_gid :: gid_t(),
	  i_rdev :: kdev_t(),
	  i_size :: off_t(),
	  i_atime :: time_t(),
	  i_mtime :: time_t(),
	  i_ctime :: time_t(),
	  i_blksize :: non_neg_integer(),
	  i_blocks :: non_neg_integer(),
	  i_version :: non_neg_integer(),
	  i_nrpages :: non_neg_integer(),
	  i_sem :: semaphore(),
	  i_op :: ptr_inode_operations(),
	  i_sb :: ptr_super_block(),
	  i_wait :: wait_queue_head_t(),
	  i_state :: non_neg_integer(),
	  i_flags :: non_neg_integer(),
	  i_sock  :: integer(),
	  i_writecount :: atomic_t(),
	  i_attr_flags :: non_neg_integer()
	}).

