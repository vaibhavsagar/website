--------------------------------------------------------------------------------
title: I Haskell a Git
published: 2017-08-13
tags: programming, haskell, git
--------------------------------------------------------------------------------

_This blog post is available in [Russian](https://flyclipart.com/ru-i-haskell-a-git) (translated by [Free clipart blog](https://flyclipart.com/))_

I struggled with Git for a long time, and every time I thought I had finally made sense of it, I would accidentally delete a repository or mess up a branch, causing me to question my grasp of what I was doing. I found it very difficult to form a mental model of the tool from the proliferation of seemingly endless command line flags that I had to use to achieve anything meaningful, and the cryptic errors that would inevitably result.

When I finally thought I understood what was going on, I offered to give a talk on it to the local functional group, because Git is functional, right? The co-organisers explained that it wouldn't be an interesting or useful talk, but a talk on implementing Git in Haskell would be very welcome.

That was enough motivation to start working on a [Git library](https://github.com/vaibhavsagar/duffer), and it turns out that understanding Git from the inside out is far, far easier than whatever I was trying to do earlier. This blog post is my attempt to share that comfort and understanding with you.

I've chosen to write this as an IHaskell notebook that is available [here](https://github.com/vaibhavsagar/notebooks/tree/master/git-from-scratch), and I've included a `default.nix` to make things easier if you have Nix installed. You should be able to run

```bash
$ $(nix-build --no-out-link)/bin/jupyter-notebook
```

to open a Jupyter notebook environment with all the dependencies you'll need to follow along.

GHCi has a handy Vim-inspired feature where a command prefixed with `:!` is run in the shell, and IHaskell supports this as well, so I'll be using that heavily to keep everything self-contained.

Let's start by picking a Git repository. I picked Ethan Schoonover's [solarized](https://github.com/altercation/solarized/) because it's nontrivial, well-known, and was last updated in 2011, so I'm confident that the hashes here won't go out of date.


```haskell
{-# LANGUAGE OverloadedStrings #-}

-- Start with a clean slate.
:!if [ -d solarized/ ]; then rm -rf solarized; fi
:!git clone https://github.com/altercation/solarized
:!cd solarized
:!git show --format=raw -s
```


    



    Cloning into 'solarized'...



    commit e40cd4130e2a82f9b03ada1ca378b7701b1a9110
    tree ecd0e58d6832566540a30dfd4878db518d5451d0
    parent ab3c5646b41de1b6d95782371289db585ba8aa85
    author Trevor Bramble <inbox@trevorbramble.com> 1372482098 -0700
    committer Trevor Bramble <inbox@trevorbramble.com> 1372482214 -0700
    
        add tmux by @seebi!


`git show` displays the latest commit on the current branch, `--format=raw` shows it in raw format, and the `-s` flag suppresses the diff output, which (as we'll see later) isn't part of the commit.

The first thing we have to address is the fact that Git has two storage formats: loose objects and packfiles. In the loose object format, each Git object is stored in its own file under the `.git/objects` directory. In the packfile format, many Git objects are stored in a file under the `.git/objects/pack` directory with an associated pack index to make lookups feasible.

Loose objects are used below a certain size threshold as an on-disk format, and packfiles are used as a space optimisation and to transfer files over the network because transferring one large file has less overhead than transferring lots of small files. Loose objects are easier to work with, so I'm going to convert the packfiles into loose objects.

If you'd like to learn more about packfiles, my favourite resource is Aditya Mukerjee's [Unpacking Git packfiles](https://codewords.recurse.com/issues/three/unpacking-git-packfiles).


```haskell
-- `git unpack-objects` doesn't do any unpacking if the objects already exist in the repository
:!mv .git/objects/pack/* .
-- Stream the packfiles to `git unpack-objects`, which splits them into individual objects and stores them appropriately
:!cat *.pack | git unpack-objects
-- We don't need the packfiles any more
:!rm -rf pack-*
```


    



    



    


Okay, the packfiles are gone and there are only loose objects now.

`git show` is an example of a 'porcelain' command for users to interact with, as opposed to a 'plumbing' command that is more low-level and meant for Git itself to use under the hood. The latest commit on the current branch is known as the `HEAD` commit, and we should be able to use `git cat-file -p` to get essentially the same output as before (the `-p` flag means 'pretty-print').


```haskell
:!git cat-file -p HEAD
```


    tree ecd0e58d6832566540a30dfd4878db518d5451d0
    parent ab3c5646b41de1b6d95782371289db585ba8aa85
    author Trevor Bramble <inbox@trevorbramble.com> 1372482098 -0700
    committer Trevor Bramble <inbox@trevorbramble.com> 1372482214 -0700
    
    add tmux by @seebi!


`HEAD` is in fact a file that lives at `.git/HEAD`. Let's view its contents.


```haskell
:!cat .git/HEAD
```


    ref: refs/heads/master


This is essentially a symlink in text. `refs/heads/master` refers to `.git/refs/heads/master`. What are its contents?


```haskell
:!cat .git/refs/heads/master
```


    e40cd4130e2a82f9b03ada1ca378b7701b1a9110


Okay, no more pointers! This is a SHA1 hash representing the commit we want. One last `git cat-file -p`...


```haskell
:!git cat-file -p e40cd4130e2a82f9b03ada1ca378b7701b1a9110
```


    tree ecd0e58d6832566540a30dfd4878db518d5451d0
    parent ab3c5646b41de1b6d95782371289db585ba8aa85
    author Trevor Bramble <inbox@trevorbramble.com> 1372482098 -0700
    committer Trevor Bramble <inbox@trevorbramble.com> 1372482214 -0700
    
    add tmux by @seebi!


As expected, we get the same output as before. On to something different: `e40cd4130e2a82f9b03ada1ca378b7701b1a9110` is a reference to an object stored at `.git/objects/e4/0cd4130e2a82f9b03ada1ca378b7701b1a9110`. The first two characters of the hash are the directory name and the 38 remaining characters are the file name underneath that directory. It's worth pointing out that all objects are stored in this format, and there's no separation between object types or anything like that.

This unusual directory structure was chosen as a tradeoff between the number of directories under `.git/objects` and the number of files under each of those directories. One approach might have been to use 40-character file names and put all objects under `.git/objects`. However, some filesystems have operations that are `O(n)` in the number of files in a directory, and working with large repositories would get very slow in this case. Another approach would have been to use the first character of the hash as the directory name, which would lead to at most 16 directories under `.git/objects`. Git settled on the first two characters, which gives us at most 256 directories.

Let's confirm that the file does exist, and then look at its contents.


```haskell
:!ls .git/objects/e4/0cd4130e2a82f9b03ada1ca378b7701b1a9110
:!cat .git/objects/e4/0cd4130e2a82f9b03ada1ca378b7701b1a9110 | xxd
```


    .git/objects/e4/0cd4130e2a82f9b03ada1ca378b7701b1a9110



    00000000: 7801 958e 6d6a 0331 0c44 fbdb a750 0ed0  x...mj.1.D...P..
    00000010: e22f d95a 2825 f40c b980 b452 e942 9d0d  ./.Z(%.....R.B..
    00000020: ae53 92db d790 5ea0 bf06 1ec3 9b59 f7d6  .S....^......Y..
    00000030: b601 31d3 d3e8 6660 ab7a 43d2 4229 6229  ..1...f`.zC.B)b)
    00000040: 983d 27af 1f9a a992 0a06 52cc 18d4 bb0b  .='.......R.....
    00000050: 773b 0f60 492b 965c 2407 b520 4517 ac14  w;.`I+.\$.. E...
    00000060: 530d 9196 d927 1426 6642 c7d7 f1b9 7738  S....'.&fB....w8
    00000070: 75fb 99f1 deb9 c997 c1eb 7696 fd76 9cd3  u.........v..v..
    00000080: 93ca 03be ac7b 7b83 90ea 3c15 fd42 f0ec  .....{{...<..B..
    00000090: abf7 6ed2 f974 d8ff 1d31 e43f 8763 5518  ..n..t...1.?.cU.
    000000a0: ed7a 03b9 c3f1 db4c b683 fb05 c805 4f81  .z.....L......O.


Git compresses these files with zlib before storing them, and we'll need to handle this. Fortunately there's a tool called `zlib-flate` (part of the `qpdf` package) that we can use.


```haskell
:!zlib-flate -uncompress < .git/objects/e4/0cd4130e2a82f9b03ada1ca378b7701b1a9110
```


    commit 248tree ecd0e58d6832566540a30dfd4878db518d5451d0
    parent ab3c5646b41de1b6d95782371289db585ba8aa85
    author Trevor Bramble <inbox@trevorbramble.com> 1372482098 -0700
    committer Trevor Bramble <inbox@trevorbramble.com> 1372482214 -0700
    
    add tmux by @seebi!


This is identical to the output of `git cat-file -p`, except for the `commit 248` at the beginning. That's a header that Git uses to tell different types of objects apart, and `248` is the content length of this particular commit. There's also a null byte after the content length that the shell is not displaying here, and this will become important when we write code to handle the header in a moment.

I'm done playing with the shell for now, and I want to write some code. The first thing I'd like to do is import some libraries and define helper functions for compresssion and decompression. Haskell's `zlib` library works with lazy bytestrings but I use strict bytestrings in the rest of this code and I don't want to keep converting back and forth, so I'll define `compress` and `decompress` accordingly.


```haskell
import qualified Codec.Compression.Zlib as Z (compress, decompress)
import           Data.ByteString.Lazy        (fromStrict, toStrict)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString        as B

compress, decompress :: ByteString -> ByteString
compress   = toStrict . Z.compress   . fromStrict
decompress = toStrict . Z.decompress . fromStrict
```

Now to recreate the `zlib-flate` output from earlier, and demonstrate the presence of that null byte in the header:


```haskell
commit <- B.readFile ".git/objects/e4/0cd4130e2a82f9b03ada1ca378b7701b1a9110"
print $ decompress commit
```


    "commit 248\NULtree ecd0e58d6832566540a30dfd4878db518d5451d0\nparent ab3c5646b41de1b6d95782371289db585ba8aa85\nauthor Trevor Bramble <inbox@trevorbramble.com> 1372482098 -0700\ncommitter Trevor Bramble <inbox@trevorbramble.com> 1372482214 -0700\n\nadd tmux by @seebi!\n"


Next, I want to make sense of this content by parsing it. I'll write parsers that take a sequence of bytes and produce values I can work with. I also want to define serialisers (or unparsers, as I like to think of them) that take those values and turn them back into the sequence of bytes we started with.

Haskell has a couple of great options for this, and I've decided to go with `attoparsec`. It does the right thing and accounts for a parsing failure by default instead of blowing up with a runtime error, but I'm pretty confident that my parsers won't fail so I'll define a helper function that gets rid of that behaviour.


```haskell
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as AC

parsed :: Parser a -> ByteString -> a
parsed parser = either error id . AC.parseOnly parser
```

Let's write our first parser! We'll start with a simple one for the header. We want some sequence of characters, a space, a number, and a null byte, and parser combinators make implementing this straightforward.


```haskell
parseHeader :: Parser (ByteString, Int)
parseHeader = do
    objectType <- AC.takeTill AC.isSpace
    AC.space
    len <- AC.decimal
    AC.char '\NUL'
    return (objectType, len)

commit <- decompress <$> B.readFile ".git/objects/e4/0cd4130e2a82f9b03ada1ca378b7701b1a9110"

parsed parseHeader commit
```


    ("commit",248)


The next parser I want is one for references. The correct way to do this is to look for 40 characters that are in the range 0-9 or a-f, but I'm lazy and I'm going to just grab 40 characters instead. Rabbit hole: write a parser that only parses valid SHA1 hashes.


```haskell
type Ref = ByteString

parseHexRef :: Parser Ref
parseHexRef = AC.take 40
```

We now have all the smaller parsers we'll need to plug together in order to parse a commit. We want to parse the `tree`, any number of `parent`s, an `author`, a `committer`, and a message. Why any number of parents? The initial commit of a repository won't have any parents, and merge commits will have at least two, although there can be more (this is known as an [octopus merge](https://www.destroyallsoftware.com/blog/2017/the-biggest-and-weirdest-commits-in-linux-kernel-git-history)).

The author and committer lines consist of a user's name, their email, the unix timestamp, and the timezone. A better parser for this would validate each of those components, but to demonstrate I'm just going to grab the whole line. Rabbit hole: write the better person+time parser.

One thing I really like about parser combinators is that I can write a parser whose form imitates the content I'm trying to parse. This is purely a cute stylistic quirk, but I enjoy doing it anyway. 


```haskell
data Commit = Commit
    { commitTree      :: Ref
    , commitParents   :: [Ref]
    , commitAuthor    :: ByteString
    , commitCommitter :: ByteString
    , commitMessage   :: ByteString
    } deriving (Eq, Show)

parseCommit = do
    cTree      <-           AC.string "tree"      *> AC.space *> parseHexRef                   <* AC.endOfLine
    cParents   <- AC.many' (AC.string "parent"    *> AC.space *> parseHexRef                   <* AC.endOfLine)
    cAuthor    <-           AC.string "author"    *> AC.space *> AC.takeTill (AC.inClass "\n") <* AC.endOfLine
    cCommitter <-           AC.string "committer" *> AC.space *> AC.takeTill (AC.inClass "\n") <* AC.endOfLine
    AC.endOfLine
    cMessage   <- AC.takeByteString
    return $ Commit cTree cParents cAuthor cCommitter cMessage

parsed (parseHeader *> parseCommit) commit
```


    Commit {commitTree = "ecd0e58d6832566540a30dfd4878db518d5451d0", commitParents = ["ab3c5646b41de1b6d95782371289db585ba8aa85"], commitAuthor = "Trevor Bramble <inbox@trevorbramble.com> 1372482098 -0700", commitCommitter = "Trevor Bramble <inbox@trevorbramble.com> 1372482214 -0700", commitMessage = "add tmux by @seebi!\n"}


Now to write our first serialiser that takes values of the Commit type and turns them back into bytestrings. Again, with some formatting liberties I can make this look a lot like the content I want to output. I can quickly check that it [round-trips](http://teh.id.au/posts/2017/06/07/round-trip-property/) to see that both my parser and serialiser work properly.


```haskell
import Data.Monoid ((<>), mappend, mconcat)
import Data.Byteable

instance Byteable Commit where
    toBytes (Commit cTree cParents cAuthor cCommitter cMessage) = mconcat
        [                        "tree "      <> cTree      <> "\n"
        , mconcat (map (\cRef -> "parent "    <> cRef       <> "\n") cParents)
        ,                        "author "    <> cAuthor    <> "\n"
        ,                        "committer " <> cCommitter <> "\n"
        ,                                                      "\n"
        ,                                        cMessage
        ]

parsedCommit = parsed (parseHeader *> parseCommit) commit
(parsed parseCommit . toBytes $ parsedCommit) == parsedCommit
```


    True


Let's backtrack and also define a serialiser for our headers.


```haskell
import Data.ByteString.UTF8 (fromString, toString)

withHeader :: ByteString -> ByteString -> ByteString
withHeader oType content = mconcat [oType, " ", fromString . show $ B.length content, "\NUL", content]

withHeader "commit" (toBytes parsedCommit)
```


    commit 248tree ecd0e58d6832566540a30dfd4878db518d5451d0
    parent ab3c5646b41de1b6d95782371289db585ba8aa85
    author Trevor Bramble <inbox@trevorbramble.com> 1372482098 -0700
    committer Trevor Bramble <inbox@trevorbramble.com> 1372482214 -0700
    
    add tmux by @seebi!


Great, it looks like that does the right thing. We'll test it more thoroughly later.

So far I've avoided the question of where the hashes come from. Git is a content-addressable store (CAS) and the content of our Git objects uniquely determines their hash. This is very much like a hash table, and that's a useful way to think about Git: a hashtable on the filesystem.

More specifically, the SHA1 hash of a Git object before compression is used as the reference. Let me demonstrate.


```haskell
import Data.Digest.Pure.SHA

hash :: ByteString -> Ref
hash = fromString . showDigest . sha1 . fromStrict

hash (withHeader "commit" (toBytes parsedCommit))
```


    e40cd4130e2a82f9b03ada1ca378b7701b1a9110


This is the same hash as the one we've been using to get at the commit so far, which is consistent with my explanation.

I think this is a good point to mention that Git commits form a [directed acyclic graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph), and this property is ensured by the way hashes are computed: since a commit hash depends on the content of the `parent` fields, a commit with an ancestor referring back to it would somehow need that ancestor (and therefore all its successors) to know the final commit hash _before it has been determined_. However, since SHA1 has recently been [broken in practice](https://shattered.io/), it might be eventually possible to generate a Git commit cycle and I'm curious to see how the tool would behave in its presence.

Now that we're done with commits, let's look at trees. A tree is what Git calls a directory listing. I think the tree reference `ecd0e58d6832566540a30dfd4878db518d5451d0` in the above commit is a good one to start with.

A tree object consists of some number of tree entries, and each tree entry represents a directory/file, with a reference to another Git object that stores the actual content of the directory/file. I think of these as [tries](https://en.wikipedia.org/wiki/Trie), with file contents at the leaves.


```haskell
:!git cat-file -p ecd0e58d6832566540a30dfd4878db518d5451d0
```


    100644 blob e69de29bb2d1d6434b8b29ae775ad8c2e48c5391	.gitmodules
    100644 blob ec00a76061539cf774614788270214499696f871	CHANGELOG.mkd
    100644 blob f95aaf80007d225f00d3109987ee42ef2c2e0c0a	DEVELOPERS.mkd
    100644 blob ee08d7e44f15108ef5359550399dad55955b56ca	LICENSE
    100644 blob d18ee9450251ea1b9a02ebd4d6fce022df9eb5e4	README.md
    040000 tree 1981c76881c6a14e14d067a44247acd1bf6bbc3a	adobe-swatches-solarized
    040000 tree 825c732bdd3a62aeb543ca89026a26a2ee0fba26	apple-colorpalette-solarized
    040000 tree 7bab2828df5de23262a821cc48fe0ccf8bd2a9ae	emacs-colors-solarized
    040000 tree f5fe8c3e20b2577223f617683a52eac31c5c9f30	files
    040000 tree 5b60111510dbb3d8560cf58a36a20a99fc175658	gedit
    040000 tree 60c9df3d6e1994b76d72c061a02639af3d925655	gimp-palette-solarized
    040000 tree 979cf43752e4d698c7b5b47cff665142a274c133	img
    040000 tree 3ff6d431303b66cc50e45b6fabd72302f210aebc	intellij-colors-solarized
    040000 tree 8f387a531ad08f146c86e4b6007b898064ad4d7f	iterm2-colors-solarized
    040000 tree 1e37592e62c85909be4c5e5eb774f177766e8422	mutt-colors-solarized
    040000 tree 8f321f917040d903f701a2b33aeee26aed2ee544	netbeans-colors-solarized
    040000 tree 0d408465820822f6a2afccf43e9627375fedc278	osx-terminal.app-colors-solarized
    040000 tree 63dfa6c40d214f8e0f76d39f7a2283e053940a19	putty-colors-solarized
    040000 tree 453921a267d3eb855e40c7de73aee46088563f3e	qtcreator
    040000 tree 5dd6832a324187f8f521bef928891fb87cf845f6	seestyle-colors-solarized
    040000 tree 3c15973ed107e7b37d1c4885f82984658ecbdf6a	textmate-colors-solarized
    040000 tree 4db152b36a47e31a872e778c02161f537888e44b	textwrangler-bbedit-colors-solarized
    040000 tree 09b5f2f69e1596c6ff66fb187ea6bdc385845152	tmux
    040000 tree 635ebbb919fcbbaf6fe958998553bf3f5fe09210	utils
    040000 tree b87a2100b0a79424cd4b2a4e4ef03274b130a206	vim-colors-solarized
    040000 tree 8dea7190b79c05404aa6a1f0d67c5c6671d66fe1	visualstudio-colors-solarized
    040000 tree 0a531826e913a4b11823ee1be6e1b367f826006f	xchat
    040000 tree 2870bdf394a6b6b3bd10c263ffe9396a0d3d3366	xfce4-terminal
    040000 tree 5d1a212e2fd9cdc2b678e3be56cf776b2f16cfe2	xresources


The number at the beginning of each entry represents the entry permissions, and is a subset of Unix file permissions. `100644` corresponds to a blob, which is the Git object corresponding to a file, and `040000` corresponds to a tree. Other numbers exist but are uncommon. The rest of the tree entry is the entry reference and the entry name.

As before, we should be able to decompress the file and get essentially the same output as before, right?


```haskell
tree <- decompress <$> B.readFile ".git/objects/ec/d0e58d6832566540a30dfd4878db518d5451d0"
print tree
```


    "tree 1282\NUL100644 .gitmodules\NUL\230\157\226\155\178\209\214CK\139)\174wZ\216\194\228\140S\145\&100644 CHANGELOG.mkd\NUL\236\NUL\167`aS\156\247taG\136'\STX\DC4I\150\150\248q100644 DEVELOPERS.mkd\NUL\249Z\175\128\NUL}\"_\NUL\211\DLE\153\135\238B\239,.\f\n100644 LICENSE\NUL\238\b\215\228O\NAK\DLE\142\245\&5\149P9\157\173U\149[V\202\&100644 README.md\NUL\209\142\233E\STXQ\234\ESC\154\STX\235\212\214\252\224\"\223\158\181\228\&40000 adobe-swatches-solarized\NUL\EM\129\199h\129\198\161N\DC4\208g\164BG\172\209\191k\188:40000 apple-colorpalette-solarized\NUL\130\\s+\221:b\174\181C\202\137\STXj&\162\238\SI\186&40000 emacs-colors-solarized\NUL{\171((\223]\226\&2b\168!\204H\254\f\207\139\210\169\174\&40000 files\NUL\245\254\140> \178Wr#\246\ETBh:R\234\195\FS\\\159\&040000 gedit\NUL[`\DC1\NAK\DLE\219\179\216V\f\245\138\&6\162\n\153\252\ETBVX40000 gimp-palette-solarized\NUL`\201\223=n\EM\148\183mr\192a\160&9\175=\146VU40000 img\NUL\151\156\244\&7R\228\214\152\199\181\180|\255fQB\162t\193\&340000 intellij-colors-solarized\NUL?\246\212\&10;f\204P\228[o\171\215#\STX\242\DLE\174\188\&40000 iterm2-colors-solarized\NUL\143\&8zS\SUB\208\143\DC4l\134\228\182\NUL{\137\128d\173M\DEL40000 mutt-colors-solarized\NUL\RS7Y.b\200Y\t\190L^^\183t\241wvn\132\"40000 netbeans-colors-solarized\NUL\143\&2\US\145p@\217\ETX\247\SOH\162\179:\238\226j\237.\229D40000 osx-terminal.app-colors-solarized\NUL\r@\132e\130\b\"\246\162\175\204\244>\150'7_\237\194x40000 putty-colors-solarized\NULc\223\166\196\r!O\142\SIv\211\159z\"\131\224S\148\n\EM40000 qtcreator\NULE9!\162g\211\235\133^@\199\222s\174\228`\136V?>40000 seestyle-colors-solarized\NUL]\214\131*2A\135\248\245!\190\249(\137\US\184|\248E\246\&40000 textmate-colors-solarized\NUL<\NAK\151>\209\a\231\179}\FSH\133\248)\132e\142\203\223j40000 textwrangler-bbedit-colors-solarized\NULM\177R\179jG\227\SUB\135.w\140\STX\SYN\USSx\136\228K40000 tmux\NUL\t\181\242\246\158\NAK\150\198\255f\251\CAN~\166\189\195\133\132QR40000 utils\NULc^\187\185\EM\252\187\175o\233X\153\133S\191?_\224\146\DLE40000 vim-colors-solarized\NUL\184z!\NUL\176\167\148$\205K*NN\240\&2t\177\&0\162\ACK40000 visualstudio-colors-solarized\NUL\141\234q\144\183\156\ENQ@J\166\161\240\214|\\fq\214o\225\&40000 xchat\NUL\nS\CAN&\233\DC3\164\177\CAN#\238\ESC\230\225\179g\248&\NULo40000 xfce4-terminal\NUL(p\189\243\148\166\182\179\189\DLE\194c\255\233\&9j\r=3f40000 xresources\NUL]\SUB!./\217\205\194\182x\227\190V\207wk/\SYN\207\226"


Although this looks very much like gibberish, it is the same content as above with one big difference: instead of the 40-byte hexadecimal representation of a SHA1 hash, the 20-byte representation is used. The `tree <length>` header is present, as is the entry permission. Each entry name is followed by a `\NUL` to facilitate parsing.

We are now able to define a parser for tree objects. Rabbit hole: the tree entries need to be sorted in a certain quirky order, and we would like to disallow duplicates. Use a different data structure and manual `Ord` definitions to ensure this.


```haskell
import Data.ByteString.Base16 (encode)

parseBinRef :: Parser Ref
parseBinRef = encode <$> AC.take 20

data Tree = Tree { treeEntries :: [TreeEntry] } deriving (Eq, Show)

data TreeEntry = TreeEntry
    { treeEntryPerms :: ByteString
    , treeEntryName  :: ByteString
    , treeEntryRef   :: Ref
    } deriving (Eq, Show)

parseTreeEntry :: Parser TreeEntry
parseTreeEntry = do
    perms <- fromString <$> AC.many1' AC.digit
    AC.space
    name  <- AC.takeWhile (/='\NUL')
    AC.char '\NUL'
    ref   <- parseBinRef
    return $ TreeEntry perms name ref

parseTree :: Parser Tree
parseTree = Tree <$> AC.many' parseTreeEntry

parsedTree = parsed (parseHeader *> parseTree) tree
parsedTree
```


    Tree {treeEntries = [TreeEntry {treeEntryPerms = "100644", treeEntryName = ".gitmodules", treeEntryRef = "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"},TreeEntry {treeEntryPerms = "100644", treeEntryName = "CHANGELOG.mkd", treeEntryRef = "ec00a76061539cf774614788270214499696f871"},TreeEntry {treeEntryPerms = "100644", treeEntryName = "DEVELOPERS.mkd", treeEntryRef = "f95aaf80007d225f00d3109987ee42ef2c2e0c0a"},TreeEntry {treeEntryPerms = "100644", treeEntryName = "LICENSE", treeEntryRef = "ee08d7e44f15108ef5359550399dad55955b56ca"},TreeEntry {treeEntryPerms = "100644", treeEntryName = "README.md", treeEntryRef = "d18ee9450251ea1b9a02ebd4d6fce022df9eb5e4"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "adobe-swatches-solarized", treeEntryRef = "1981c76881c6a14e14d067a44247acd1bf6bbc3a"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "apple-colorpalette-solarized", treeEntryRef = "825c732bdd3a62aeb543ca89026a26a2ee0fba26"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "emacs-colors-solarized", treeEntryRef = "7bab2828df5de23262a821cc48fe0ccf8bd2a9ae"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "files", treeEntryRef = "f5fe8c3e20b2577223f617683a52eac31c5c9f30"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "gedit", treeEntryRef = "5b60111510dbb3d8560cf58a36a20a99fc175658"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "gimp-palette-solarized", treeEntryRef = "60c9df3d6e1994b76d72c061a02639af3d925655"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "img", treeEntryRef = "979cf43752e4d698c7b5b47cff665142a274c133"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "intellij-colors-solarized", treeEntryRef = "3ff6d431303b66cc50e45b6fabd72302f210aebc"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "iterm2-colors-solarized", treeEntryRef = "8f387a531ad08f146c86e4b6007b898064ad4d7f"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "mutt-colors-solarized", treeEntryRef = "1e37592e62c85909be4c5e5eb774f177766e8422"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "netbeans-colors-solarized", treeEntryRef = "8f321f917040d903f701a2b33aeee26aed2ee544"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "osx-terminal.app-colors-solarized", treeEntryRef = "0d408465820822f6a2afccf43e9627375fedc278"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "putty-colors-solarized", treeEntryRef = "63dfa6c40d214f8e0f76d39f7a2283e053940a19"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "qtcreator", treeEntryRef = "453921a267d3eb855e40c7de73aee46088563f3e"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "seestyle-colors-solarized", treeEntryRef = "5dd6832a324187f8f521bef928891fb87cf845f6"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "textmate-colors-solarized", treeEntryRef = "3c15973ed107e7b37d1c4885f82984658ecbdf6a"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "textwrangler-bbedit-colors-solarized", treeEntryRef = "4db152b36a47e31a872e778c02161f537888e44b"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "tmux", treeEntryRef = "09b5f2f69e1596c6ff66fb187ea6bdc385845152"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "utils", treeEntryRef = "635ebbb919fcbbaf6fe958998553bf3f5fe09210"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "vim-colors-solarized", treeEntryRef = "b87a2100b0a79424cd4b2a4e4ef03274b130a206"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "visualstudio-colors-solarized", treeEntryRef = "8dea7190b79c05404aa6a1f0d67c5c6671d66fe1"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "xchat", treeEntryRef = "0a531826e913a4b11823ee1be6e1b367f826006f"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "xfce4-terminal", treeEntryRef = "2870bdf394a6b6b3bd10c263ffe9396a0d3d3366"},TreeEntry {treeEntryPerms = "40000", treeEntryName = "xresources", treeEntryRef = "5d1a212e2fd9cdc2b678e3be56cf776b2f16cfe2"}]}


It's similarly straightforward to define a serialiser. All we have to do is serialise the tree entries and concatenate them.


```haskell
import Data.ByteString.Base16 (decode)

instance Byteable TreeEntry where
    toBytes (TreeEntry perms name ref) = mconcat [perms, " ", name, "\NUL", fst $ decode ref]

instance Byteable Tree where
    toBytes (Tree entries) = mconcat (map toBytes entries)

(parsed parseTree . toBytes $ parsedTree) == parsedTree
```


    True


Next we move to blobs. I'm using the reference associated with `CHANGELOG.mkd` because `.gitmodules` is empty, and limiting the output to the first ten lines for now because we'll see the whole thing later anyway.


```haskell
:!git cat-file -p ec00a76061539cf774614788270214499696f871 | head -n10
```


    Solarized Changelog
    ===================
    
    ## Current release 1.0.0beta2
    
    1.0.0beta2
    ----------
    
    ### Summary


A blob is some bytes with a header.


```haskell
import qualified Data.ByteString.Char8 as BC
blob <- decompress <$> B.readFile ".git/objects/ec/00a76061539cf774614788270214499696f871"
print $ BC.unlines . take 10 . BC.lines $ blob
```


    "blob 5549\NULSolarized Changelog\n===================\n\n## Current release 1.0.0beta2\n\n1.0.0beta2\n----------\n\n### Summary\n\n"


Parsing blobs is easy!


```haskell
data Blob = Blob { blobContent :: ByteString } deriving (Eq, Show)

parseBlob :: Parser Blob
parseBlob = Blob <$> AC.takeByteString

parsedBlob = parsed (parseHeader *> parseBlob) blob
parsedBlob
```


    Blob {blobContent = "Solarized Changelog\n===================\n\n## Current release 1.0.0beta2\n\n1.0.0beta2\n----------\n\n### Summary\n\nSwitch to the alternative red hue (final and only hue change), included a whole\nheap of new ports and updates to the existing Vim colorscheme. The list of all \ncurrently included ports, highlighted items are new, updates noted:\n\n#### Editors & IDEs\n\n*   \\[UPDATED\\] **Vim**\n*   \\[NEW\\] ***Emacs***\n*   \\[NEW\\] ***IntelliJ IDEA***\n*   \\[NEW\\] ***NetBeans***\n*   \\[NEW\\] ***SeeStyle theme for Coda & SubEthaEdit***\n*   \\[NEW\\] ***TextMate***\n*   \\[NEW\\] ***Visual Studio***\n\n#### Terminal Emulators\n\n* \\[UPDATED\\] **iTerm2 colorschemes**\n* \\[UPDATED\\] **OS X Terminal.app colors**\n* \\[UPDATED\\] **Xresources colors**\n\n#### Other Applications\n\n* \\[UPDATED\\] **Mutt mail client colorschemes**\n\n#### Palettes\n\n* \\[UPDATED\\] **Adobe Photoshop Swatches**\n* \\[UPDATED\\] **Apple Color Picker Palette**\n* \\[UPDATED\\] **Gimp Palette**\n\n\n### Critical Changes\n\nThese changes may require you to change your configuration.\n\n*   **GLOBAL : IMPROVEMENT : New red accent color value**\n    Modified red from L\\*a\\*b lightness value 45 to 50 to bring it in\n    line with the other accent colors and address bleed into dark background on \n    some displays, as well as reducing shift of red against base03 when viewed \n    with glasses (chromatic aberration). All instances of the colorscheme and \n    palettes updated to new red and avalailable for use/import without further \n    modification. Forks and ports should pull new changes and/or update ported \n    red value accordingly.  The new red:\n\n            red #dc322f\n\n*   **VIM : CHANGE : Default mode now 16 color**\n    Default terminal mode is now ***16 colors***. Most of the users of terminal \n    mode seem comfortabel and capable changing terminal colors. This is the \n    preferred method of implementing Solarized in Terminal mode. If you wish to \n    instead use the degraded 256 color palette, you may do so with the \n    following line in your .vimrc:\n\n            let g:solarized_termcolors=256\n\n    You no longer need to specify \"let g:solarized_termcolors=16\" as it is now \n    the default; leaving it in your .vimrc won't hurt anything, however.\n\n*   **VIM : IMPROVEMENT : New Toggle Background Plugin**\n    Added new Toggle Background plugin. Will load automatically and show up as \n    a menu item in the `Window` menu in gui vim. Automatically maps to\n    `<F5>` if available (won't clobber that mapping if you're using it).\n    Also available as a command `:ToggleBG`.  To manually map to\n    something other than `<F5>`:\n\n    To set your own mapping in your .vimrc file, simply add the following line \n    to support normal, insert and visual mode usage, changing the\n    \"`<F5>`\" value to the key or key combination you wish to use:\n\n        call togglebg#map(\"<F5>\")\n\n    Note that you'll want to use a single function key or equivalent if you want \n    the plugin to work in all modes (normal, insert, visual).\n\n*   **VIM : IMPROVEMENT : Special & Non-text items now more visible**\n    Special characters such as trailing whitespace, tabs, newlines, when \n    displayed using \":set list\" can be set to one of three levels depending on \n    your needs.\n\n            let g:solarized_visibility =  \"normal\"|   \"high\" or \"low\"\n\n    I'll be honest: I still prefer low visibility. I like them barely there.  \n    They show up in lines that are highlighted as by the cursor line, which \n    works for me. If you are with me on this, put the following in your .vimrc:\n\n            let g:solarized_visibility =  \"low\"\n\n### Non Critical Changes\n\nThese changes should not impact your usage of the Solarized.\n\n*   **PALETTES : IMPROVEMENT : Colorspace tagged and untagged versions**\n    Changed default OS X color picker palatte swatches to tagged colors (sRGB) \n    and included alternate palette with untagged color swatches for advanced \n    users (v1.0.0beta1 had untagged as default).\n\n*   **VIM : BUGFIX : Better display in Terminal.app, other emulators**\n    Terminal.app and other common terminal emulators that report 8 color mode \n    had display issues due to order of synt highlighting definitions and color \n    values specified. These have been conformed and reordered in such a way \n    that there is a more graceful degrading of the Solarized color palette on \n    8 color terminals. Infact, the experience should be almost identical to gui \n    other than lack of bold typeface.\n\n*   **VIM : BUGFIX : Better distinction between status bar and split windows**\n    Status bar was previously too similar to the cursor line and window splits.  \n    This has now been changed significantly to improve the clarity of what is \n    status, cursor line and window separator.\n\n*   **VIM : STREAMLINED : Removed simultaneous gui/cterm definitions**\n*   Refactored solarized.vim to eliminate simultaneous definition of gui and \n    cterm values.\n\n*   **VIM : BUGFIX : Removed italicized front in terminal mode**\n    Removed default italicized font in terminal mode in the Solarized Vim\n    colorscheme (many terminal emulators display Vim italics as reversed type).  \n    Italics still used in GUI mode by default and can still be turned off in \n    both modes by setting a variable: `let g:solarized_italic=0`.\n\n1.0.0beta1\n----------\n\nFirst public release. Included:\n\n* Adobe Photoshop Swatches\n* Apple Color Picker Palette\n* Gimp Palette\n* iTerm2 colorschemes\n* Mutt mail client colorschemes\n* OS X Terminal.app colors\n* Vim Colorscheme\n* Xresources colors\n\n\n\n***\n\nMODIFIED: 2011 Apr 16\n"}


As is serialising them.


```haskell
instance Byteable Blob where
    toBytes (Blob content) = content

(parsed parseBlob . toBytes $ parsedBlob) == parsedBlob
```


    True


Finally we move to Git tags, which are a way to associate a name with a reference. Git has a handy `show-ref --tags` command we can use to list them:


```haskell
:!git show-ref --tags
```


    31ff7f5064824d2231648119feb6dfda1a3c89f5 refs/tags/v1.0.0beta1
    a3037b428f29f0c032aeeeedb4758501bc32444d refs/tags/v1.0beta


There are two types of tags: lightweight tags and annotated tags. Lightweight tags are just files very much like `refs/heads/master` containing a ref, and annotated tags have a message associated with them like a commit. Only annotated tags are Git objects.


```haskell
:!git cat-file -p 31ff7f5064824d2231648119feb6dfda1a3c89f5
```


    object 90581c7bfbcd279768580eec595d0ab3c094cc02
    type commit
    tag v1.0.0beta1
    tagger Ethan Schoonover <es@ethanschoonover.com> 1300994142 -0700
    
    Initial public beta release 1.0.0beta1


Although tags are mostly used with commits, it's possible to tag any Git object. You can even tag another tag, although I can't see why you might want to.


```haskell
tag <- decompress <$> B.readFile ".git/objects/31/ff7f5064824d2231648119feb6dfda1a3c89f5"
print tag
```


    "tag 182\NULobject 90581c7bfbcd279768580eec595d0ab3c094cc02\ntype commit\ntag v1.0.0beta1\ntagger Ethan Schoonover <es@ethanschoonover.com> 1300994142 -0700\n\nInitial public beta release 1.0.0beta1\n"


Our parser for these is very similar to our commit parser. I've taken a quick break from my 'write the worst parser possible' strategy to make sure that our tags can only tag objects of type 'commit', 'tree', 'blob', or 'tag'.


```haskell
data Tag = Tag
    { tagObject     :: Ref
    , tagType       :: ByteString
    , tagTag        :: ByteString
    , tagTagger     :: ByteString
    , tagAnnotation :: ByteString
    } deriving (Eq, Show)

parseTag :: Parser Tag
parseTag = do
    tObject     <- AC.string "object" *> AC.space *> parseHexRef                                                 <* AC.endOfLine
    tType       <- AC.string "type"   *> AC.space *> AC.choice (map AC.string ["commit", "tree", "blob", "tag"]) <* AC.endOfLine
    tTag        <- AC.string "tag"    *> AC.space *> AC.takeTill (AC.inClass "\n")                               <* AC.endOfLine
    tTagger     <- AC.string "tagger" *> AC.space *> AC.takeTill (AC.inClass "\n")                               <* AC.endOfLine
    AC.endOfLine
    tAnnotation <- AC.takeByteString
    return $ Tag tObject tType tTag tTagger tAnnotation

parsedTag = parsed (parseHeader *> parseTag) tag
parsedTag
```


    Tag {tagObject = "90581c7bfbcd279768580eec595d0ab3c094cc02", tagType = "commit", tagTag = "v1.0.0beta1", tagTagger = "Ethan Schoonover <es@ethanschoonover.com> 1300994142 -0700", tagAnnotation = "Initial public beta release 1.0.0beta1\n"}


Our last serialiser follows.


```haskell
instance Byteable Tag where
    toBytes (Tag tObject tType tTag tTagger tAnnotation) = mconcat
        [ "object " <> tObject     <> "\n"
        , "type "   <> tType       <> "\n"
        , "tag "    <> tTag        <> "\n"
        , "tagger " <> tTagger     <> "\n"
        ,                             "\n"
        ,              tAnnotation
        ]

(parsed parseTag . toBytes $ parsedTag) == parsedTag
```


    True


Okay, now to bring it all together. We can define an umbrella `GitObject` type and the associated parser, serialiser, and hasher for it.


```haskell
data GitObject
    = GitCommit Commit
    | GitTree   Tree
    | GitBlob   Blob
    | GitTag    Tag
    deriving (Eq, Show)

parseGitObject :: Parser GitObject
parseGitObject = do
    headerLen <- parseHeader
    case (fst headerLen) of
        "commit" -> GitCommit <$> parseCommit
        "tree"   -> GitTree   <$> parseTree
        "blob"   -> GitBlob   <$> parseBlob
        "tag"    -> GitTag    <$> parseTag
        _        -> error "not a git object"

instance Byteable GitObject where
    toBytes obj = case obj of
        GitCommit c -> withHeader "commit" (toBytes c)
        GitTree   t -> withHeader "tree"   (toBytes t)
        GitBlob   b -> withHeader "blob"   (toBytes b)
        GitTag    t -> withHeader "tag"    (toBytes t)

hashObject :: GitObject -> Ref
hashObject = hash . toBytes
```

Let's do a quick test to check that our definitions work.


```haskell
hashObject . parsed parseGitObject . decompress <$> B.readFile ".git/objects/31/ff7f5064824d2231648119feb6dfda1a3c89f5"
```


    31ff7f5064824d2231648119feb6dfda1a3c89f5


Excellent, although we are lacking a helper to turn a reference into a Git object filepath. Let's define that.


```haskell
import System.FilePath ((</>))

refPath :: FilePath -> Ref -> FilePath
refPath gitDir ref = let
   (dir,file) = splitAt 2 (toString ref)
   in gitDir </> "objects" </> dir </> file

refPath ".git" "31ff7f5064824d2231648119feb6dfda1a3c89f5"
```


    ".git/objects/31/ff7f5064824d2231648119feb6dfda1a3c89f5"


Now we can define a `readObject` action that takes a reference and returns a parsed Git object.


```haskell
readObject :: FilePath -> Ref -> IO GitObject
readObject gitDir ref = do
    let path = refPath gitDir ref
    content <- decompress <$> B.readFile path
    return $ parsed parseGitObject content

readObject ".git" "31ff7f5064824d2231648119feb6dfda1a3c89f5"
```


    GitTag (Tag {tagObject = "90581c7bfbcd279768580eec595d0ab3c094cc02", tagType = "commit", tagTag = "v1.0.0beta1", tagTagger = "Ethan Schoonover <es@ethanschoonover.com> 1300994142 -0700", tagAnnotation = "Initial public beta release 1.0.0beta1\n"})


Next we define a `writeObject` action that takes a Git object and stores it under the right path if it doesn't already exist. The "doesn't already exist" bit is the magic of Git: we can safely assume that an object with the same hash is the same object. Every time a tree or blob changes, only the changed objects are written to the disk, and this is how Git manages to be space-efficient.


```haskell
import System.Directory (doesPathExist, createDirectoryIfMissing)
import System.FilePath  (takeDirectory)
import Control.Monad    (when, unless)

writeObject :: FilePath -> GitObject -> IO Ref
writeObject gitDir object = do
    let ref  =  hashObject object
    let path =  refPath gitDir ref
    exists   <- doesPathExist path
    unless exists $ do
        let dir = takeDirectory path
        createDirectoryIfMissing True dir
        B.writeFile path . compress $ toBytes object
    return ref
```

Okay, time for the grand finale! We're going to read and then write every object in this Git repository. If we've implemented everything correctly, the number of references before and after will be unchanged, and they will be the same references.


```haskell
import Data.Traversable (for)
import System.Directory (listDirectory)

allRefs <- do
    prefixes <- filter (\d -> length d == 2) <$> listDirectory ".git/objects/"
    concat <$> for prefixes (\p ->
        map (fromString . (p++)) <$> listDirectory (".git/objects" </> p))

print $ length allRefs

test <- for allRefs $ \ref -> do
    obj  <- readObject  ".git" ref
    ref' <- writeObject ".git" obj
    return $ ref == ref'

and test

allRefs' <- do
    prefixes <- filter (\d -> length d == 2) <$> listDirectory ".git/objects/"
    concat <$> for prefixes (\p ->
        map (fromString . (p++)) <$> listDirectory (".git/objects" </> p))

print $ length allRefs'

allRefs == allRefs'
```


    2186



    True



    2186



    True


And that's essentially all there is to Git! I've skipped over most of the additional quirks, features, and optimisations but I hope I've established that even with the relatively small amount of code above you can implement a working and usable Git API.

You'll notice that one thing I haven't mentioned at all is diffing or merging. That's because Git doesn't store diffs! They are computed on the fly when you ask for them. The packfile format does do diffing as a space optimisation, but I think it's important to point out that you can have a perfectly cromulent implementation without them because that is what surprised me the most when I learned this for the first time.

A good mental model of Git empowered me to use it better. I'd heard that binary files and Git don't go well together, but I only understood why recently: Git stores every version of every file, and binary files don't compress very well (unlike text files), so they take up huge amounts of space. I'd also read about [CocoaPods causing issues for GitHub](https://github.com/CocoaPods/CocoaPods/issues/4989#issuecomment-193772935), and now I know that this is because the tree objects representing the `Specs` directory were very large and constantly getting updated, leading to a lot of stress on GitHub's servers.

What else can you do with this power? You can...

- [Craft your own repositories](https://github.com/vaibhavsagar/git-internals-workshop)
- Run analytics on your commit graph when `git log` won't cut it!
- [Write a web API for your repository!](https://github.com/vaibhavsagar/suppandi)
- [Use Git as the backend of your application and get diffing/merging for free!](https://github.com/mirage/irmin)

The possibilities are endless!


If you'd like to learn more, you're in luck! writing on this topic is plentiful and of extremely high quality. I started with the Git Book's [chapter on Git Internals](https://git-scm.com/book/en/v2/Git-Internals-Git-Objects) and referred frequently to Vincent Hanquez's [hs-git](https://github.com/vincenthz/hs-git/) and Stefan Saasen's [overwhelmingly thorough article](http://stefan.saasen.me/articles/git-clone-in-haskell-from-the-bottom-up/) that implements enough of Git to do a `git clone` (!). Other resources include Mary Rose Cook's excellent [Git from the inside out](https://maryrosecook.com/blog/post/git-from-the-inside-out) and [Gitlet](http://gitlet.maryrosecook.com/docs/gitlet.html) as well as John Wiegley's [Git from the Bottom Up](https://jwiegley.github.io/git-from-the-bottom-up/). If nothing else, I hope the sheer proliferation of Git innards writing is enough to convince you that this is a useful and rewarding approach to learning about it.

Thanks to [Annie Cherkaev](https://anniecherkaev.com/), [Iain McCoy](https://fineshambles.com/), [Jaseem Abid](https://jaseemabid.github.io/), [Jason Shipman](https://jship.github.io/), [Tim Humphries](http://teh.id.au/), and [Tomislav Viljetic](https://viljetic.de/) for comments, clarification, and suggestions.
