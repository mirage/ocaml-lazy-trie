(*
 * Copyright (c) 2012-2013 Louis Gesbert <meta@antislash.info>
 * Copyright (c) 2012-2013 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Lazy tries based on lists *)

type ('a, 'b) t
(** Type of tries mapping from ['a list] to ['b] *)

val empty: ('a,'b) t

val create :
  ?children: ('a * ('a,'b) t) list Lazy.t ->
  ?value: 'b ->
  unit ->
  ('a,'b) t
(** Create a new trie with the given components *)

val mem: ('a,'b) t -> 'a list -> bool
(** Returns true if there is a value associated with the given path *)

val find: ('a, 'b) t -> 'a list -> 'b
(** Returns the value associated with the given path.
    @raise [Not_found] *)

val set: ('a, 'b) t -> 'a list -> 'b -> ('a, 'b) t
(** Associates a value with the given path, or replaces if there was already
    one *)

val set_lazy: ('a, 'b) t -> 'a list -> 'b Lazy.t -> ('a, 'b) t
(** The same but taking a lazy value *)

val unset: ('a, 'b) t -> 'a list -> ('a, 'b) t
(** Removes an association from the trie. Warning: doesn't cleanup branches that
    don't point to anything anymore *)

val map_subtree :
  ('a, 'b) t -> 'a list ->
  (('a, 'b) t -> ('a, 'b) t) ->
  ('a, 'b) t
(** [map_subtree tree path f] applies [f] on value and children of the node
    found at [path] in [tree], and bind the returned node back at that position
    in the tree *)

val iter: ('a list -> 'b -> unit) -> ('a, 'b) t -> unit
(** iters over all the bindings in the trie, top-down *)

val fold: ('acc -> 'a list -> 'b -> 'acc) -> ('a, 'b) t -> 'acc -> 'acc
(** folds over all bindings of the trie, bottom-up *)

val map_filter_values: ('b -> 'c option) -> ('a,'b) t -> ('a,'c) t
(** Maps and filters over all values in the trie, removing the value if [None]
    is returned *)

val sub: ('a, 'b) t -> 'a list -> ('a,'b) t
(** [sub t p] returns the sub-trie associated with the path [p] in the trie
    [t].  If [p] is not a valid path of [t], it returns an empty trie. *)

val filter_keys: ('a -> bool) -> ('a, 'b) t -> ('a, 'b) t
(** [filter f t] returns t with all subtrees for which [f key = false] pruned *)

val graft: ('a, 'b) t -> 'a list -> ('a, 'b) t -> ('a, 'b) t
(** [graft tree path node] grafts [node] in [tree] at [path] *)

val graft_lazy: ('a, 'b) t -> 'a list -> ('a, 'b) t Lazy.t -> ('a, 'b) t
(** Same as [graft], but using lazy parameters. *)
