
type t =
    | Extension of Foo.t
    | Alternate
    | Appendix
    | Bookmark
    | Chapter
    | Contents
    | Copyright
    | Current
    | Described_by
    | Edit
    | Edit_media
    | Enclosure
    | First
    | Glossary
    | Help
    | Hub
    | Index
    | Last
    | Latest_version
    | License
    | Next
    | Next_archive
    | Payment
    | Predecessor_version
    | Prev
    | Prev_archive
    | Related
    | Replies
    | Section
    | Self
    | Service
    | Start
    | Stylesheet
(*
    | Subsection
    | Successor_version
    | Up
    | Version_history
    | Via
    | Working_copy
    | Working_copy_of
*)
[@@deriving compare]

