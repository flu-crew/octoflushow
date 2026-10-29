/**
 * Parse Newick string into a tree object
 */
function parseNewick(newick) {
  let tokens = newick.split(/\s*(;|\(|\)|,|:)\s*/);
  let stack = [];
  let tree = {};
  let current = tree;

  for (let token of tokens) {
    if (token === "(") {
      let newNode = {};
      if (!current.children) current.children = [];
      current.children.push(newNode);
      stack.push(current);
      current = newNode;
    } else if (token === ",") {
      let newNode = {};
      stack[stack.length - 1].children.push(newNode);
      current = newNode;
    } else if (token === ")") {
      current = stack.pop();
    } else if (token === ":") { // ignore, length handled in next token
    } else if (token === ";") { // end of tree
    } else if (token && !isNaN(parseFloat(token))) {
      current.length = parseFloat(token);
    } else if (token) {
      current.name = token;
    }
  }
  return tree;
}

/**
 * Convert tree object back to Newick string
 */
function toNewick(node) {
  let result = "";
  if (node.children && node.children.length > 0) {
    result += "(" + node.children.map(toNewick).join(",") + ")";
  }
  if (node.name) result += node.name;
  if (node.length !== undefined) result += ":" + node.length;
  return result;
}

/**
 * Prune tree to keep only tips in allowed list
 */
function pruneTree(node, allowedTips) {
  if (!node.children || node.children.length === 0) {
    // Leaf node
    return allowedTips.has(node.name) ? node : null;
  }
  // Internal node
  node.children = node.children
    .map(child => pruneTree(child, allowedTips))
    .filter(child => child !== null);

  // If no children remain, prune this node too
  if (node.children.length === 0) return null;
  return node;
}

/**
 * Main function to subsample tips from tree from newick string
 */
function subsampleTree(newickStr, tipsToKeep) {
  // Parse tree
  let tree = parseNewick(newickStr);
//    console.log(tree); //?

  // Prune tree
  let prunedTree = pruneTree(tree, new Set(tipsToKeep));

  // Output pruned Newick
  if (prunedTree) {
    output = toNewick(prunedTree) + ";";
    return output;
  } else {
    console.log("No tips remain after pruning.");
  }
}