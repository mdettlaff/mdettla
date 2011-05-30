/*
 * Copyright 2007 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package mdettla.directorytree;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

/**
 * @author ycoppel@google.com (Yohann Coppel)
 * 
 * @param <T> Object's type in the tree.
 */
public class Tree<T> {

	private T head;
	private LinkedList<Tree<T>> leafs = new LinkedList<Tree<T>>();
	private Tree<T> parent = null;
	private Map<T, Tree<T>> locate = new HashMap<T, Tree<T>>();

	public Tree(T head) {
		this.head = head;
		locate.put(head, this);
	}

	public void addLeaf(T root, T leaf) {
		if (locate.containsKey(root)) {
			locate.get(root).addLeaf(leaf);
		} else {
			addLeaf(root).addLeaf(leaf);
		}
	}

	public Tree<T> addLeaf(T leaf) {
		Tree<T> t = new Tree<T>(leaf);
		leafs.add(t);
		t.parent = this;
		t.locate = this.locate;
		locate.put(leaf, t);
		return t;
	}

	public Tree<T> getParent() {
		return parent;
	}

	public LinkedList<Tree<T>> getSubTrees() {
		return leafs;
	}

	@Override
	public String toString() {
		return printTree("");
	}

	private String printTree(String leftSide) {
		StringBuilder result = new StringBuilder();
		String childLeftSide = "";
		if (getParent() != null) {
			result.append(leftSide + (isLastSibling() ? "└" : "├") + "── ");
			childLeftSide = leftSide + (isLastSibling() ? " " : "│") + "   ";
		}
		result.append(head);
		for (Tree<T> child : leafs) {
			result.append("\n" + child.printTree(childLeftSide));
		}
		return result.toString();
	}

	private boolean isLastSibling() {
		return getParent() == null
			|| equals(getParent().getSubTrees().getLast());
	}
}
