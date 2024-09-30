/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

var parse = require('hermes-parser').parse;
var vlq = require('vlq');

/**
 * Given a string JavaScript source which contains Flow types, return a string
 * which has removed those types.
 *
 * Options:
 *
 *   - all: (default: false)
 *     If true, bypasses looking for an @flow pragma comment before parsing.
 *
 *   - pretty: (default: false)
 *     If true, removes types completely rather than replacing with spaces.
 *     This may require using source maps.
 *
 *   - ignoreUninitializedFields: (default: false)
 *     If true, removes uninitialized class fields (`foo;`, `foo: string;`)
 *     completely rather than only removing the type. THIS IS NOT SPEC
 *     COMPLIANT! Instead, use `declare foo: string;` for type-only fields.
 *
 * Returns an object with two methods:
 *
 *   - .toString()
 *     Returns the transformed source code.
 *
 *   - .generateMap()
 *     Returns a v3 source map.
 */
module.exports = function flowRemoveTypes(source, options) {
  // Options
  var all = Boolean(options && options.all);
  if (options && options.checkPragma) {
    throw new Error(
      'flow-remove-types: the "checkPragma" option has been replaced by "all".',
    );
  }

  // If there's a @flow pragma, then skip the file
  var pragmaStart = source.indexOf('@flow');
  var pragmaSize = 5;
  if (pragmaStart !== -1) {
    return resultPrinter(options, source);
  }

  // If there's an @noflow pragma, then try to remove it.
  var pragmaStart = source.indexOf('@noflow');
  var pragmaSize = 7;

  // This parse configuration is intended to be as permissive as possible.
  var ast = parse(source, {types: true, tokens: true});

  var affectedNodes = [];

  var context = {
    ast: ast,
    source: source,
    affectedNodes,
    pretty: Boolean(options && options.pretty),
    ignoreUninitializedFields: Boolean(
      options && options.ignoreUninitializedFields,
    ),
  };

  // Remove the @noflow pragma.
  if (pragmaStart !== -1) {
    var comments = getComments(ast);
    var pragmaIdx = findTokenIndex(comments, pragmaStart);
    if (pragmaIdx >= 0 && pragmaIdx < comments.length) {
      var pragmaType = comments[pragmaIdx].type;
      if (pragmaType === 'Line' || pragmaType === 'Block') {
        affectedNodes.push({
          type: 'remove',
          node: getPragmaNode(context, pragmaStart, pragmaSize)
        });
      }
    }
  }

  // Remove all flow type definitions.
  visit(ast, context, removeFlowVisitor);

  return resultPrinter(options, source, affectedNodes);
};

function resultPrinter(options, source, affectedNodes) {
  // Options
  var pretty = Boolean(options && options.pretty);

  return {
    toString: function () {
      if (!affectedNodes || affectedNodes.length === 0) {
        return source;
      }

      var result = '';
      var lastPos = 0;

      // Step through the removed nodes, building up the resulting string.
      for (var i = 0; i < affectedNodes.length; i++) {
        var {
          type: affectType,
          node
        } = affectedNodes[i];
        result += source.slice(lastPos, startOf(node));
        lastPos = endOf(node);
        if (typeof node.__spliceValue === 'string') {
          result += node.__spliceValue;
        }

        if (affectType === 'comment') {
          result += '/* ';
          result += source.slice(startOf(node), endOf(node)).replace(/^:/, '').trim();
          result += ' */';
        } else if (affectType === 'remove') {
          // Do nothing.
        } else {
          throw new Error('Unknown affect type: ' + affectType);
        }
      }

      return (result += source.slice(lastPos));
    },
    generateMap: function () {
      return {
        version: 3,
        sources: ['source.js'],
        names: [],
        mappings: pretty ? generateSourceMappings(affectedNodes) : '',
      };
    },
  };
}

// Append node to the list of removed nodes, ensuring the order of the nodes
// in the list.
const makeChangeNode = (shouldComment, checkComment) => (context, node) => {
  var source = context.source;
  var start = startOf(node);
  if (checkComment && source[start] === '/') {
    return false;
  }

  var affectedNodes = context.affectedNodes;
  var length = affectedNodes.length;
  var index = length;

  // Check for line's leading and trailing space to be removed.
  var spaceOption = context.pretty
    ? {
      type: 'remove',
      node: getLeadingSpaceNode(context, node),
    }
    : null;
  var lineOption = context.pretty
    ? {
      type: 'remove',
      node: getTrailingLineNode(context, node)
    }
    : null;

  while (index > 0 && endOf(affectedNodes[index - 1].node) > startOf(node)) {
    index--;
  }

  const mainNodeOption = {
    type: shouldComment
      ? 'comment'
      : 'remove',
    node: node,
  }

  const newOptions = [
    spaceOption,
    mainNodeOption,
    lineOption
  ].filter((o) => o && o.node);

  if (index === length) {
    affectedNodes.push(...newOptions);
  } else {
    affectedNodes.splice(index, 0, ...newOptions);
  }

  return false;
}

const nodeIsTypeImport = (node) => {
  return node.importKind === 'type' || node.importKind === 'typeof';
}

const allSpecifiersAreType = ({ specifiers }) => {
  return specifiers.reduce((acc, node) => acc && nodeIsTypeImport(node), true);
}

var LINE_RX = /(\r\n?|\n|\u2028|\u2029)/;

var THIS_PARAM_MARKER = '__THIS_PARAM_MARKER__';

// A collection of methods for each AST type names which contain Flow types to
// be removed.
var removeFlowVisitor = {
  DeclareClass: makeChangeNode(true, true),
  DeclareFunction: makeChangeNode(true, true),
  DeclareInterface: makeChangeNode(true, true),
  DeclareModule: makeChangeNode(true, true),
  DeclareNamespace: makeChangeNode(true, true),
  DeclareTypeAlias: makeChangeNode(true, true),
  DeclareVariable: makeChangeNode(true, true),
  InterfaceDeclaration: makeChangeNode(true, true),
  TypeAlias: makeChangeNode(true, true),
  TypeAnnotation: makeChangeNode(true, true),
  TypePredicate: makeChangeNode(true, true),
  TypeParameterDeclaration: makeChangeNode(true, true),
  TypeParameterInstantiation: makeChangeNode(true, true),
  InferredPredicate: removeInferredPredicateNode,
  OpaqueType: makeChangeNode(true, true),
  DeclareOpaqueType: makeChangeNode(true, true),
  DeclareExportDeclaration: makeChangeNode(true, true),

  // ClassDeclaration: removeImplementedInterfaces,
  // ClassExpression: removeImplementedInterfaces,

  // AsExpression: function (context, node, ast) {
  //   var typeIdx = findTokenIndexAtStartOfNode(ast.tokens, node.typeAnnotation);
  //   makeChangeNode(true, false)(context, ast.tokens[typeIdx - 1]); // `as` token
  //   makeChangeNode(true, false)(context, node.typeAnnotation);
  // },

  // AsConstExpression: function (context, node, ast) {
  //   var idx = findTokenIndexAtEndOfNode(ast.tokens, node.expression);
  //   makeChangeNode(true, false)(context, ast.tokens[idx + 1]); // `as` token
  //   makeChangeNode(true, false)(context, ast.tokens[idx + 2]); // `const` token
  // },

  Identifier: function (context, node, ast) {
    if (node[THIS_PARAM_MARKER] === true) {
      makeChangeNode(true, false)(context, node);
      removeTrailingCommaNode(context, node);
      return false;
    }
    if (node.optional) {
      // Find the optional token.
      var idx = findTokenIndexAtStartOfNode(ast.tokens, node);
      do {
        idx++;
      } while (getLabel(ast.tokens[idx]) !== '?');
      makeChangeNode(true, false)(context, ast.tokens[idx]);
    }
  },

  FunctionDeclaration: function (context, node) {
    if (node.params && node.params.length) {
      if (
        node.params[0].type === 'Identifier' &&
        node.params[0].name === 'this'
      ) {
        node.params[0][THIS_PARAM_MARKER] = true;
      }
    }
  },

  FunctionExpression: function (context, node) {
    if (node.params && node.params.length) {
      if (
        node.params[0].type === 'Identifier' &&
        node.params[0].name === 'this'
      ) {
        node.params[0][THIS_PARAM_MARKER] = true;
      }
    }
  },

  PropertyDefinition: function (context, node) {
    if (node.declare || (context.ignoreUninitializedFields && !node.value)) {
      return makeChangeNode(true, false)(context, node);
    }
    if (node.variance != null) {
      makeChangeNode(true, false)(context, node.variance);
    }
  },

  ExportNamedDeclaration: function (context, node) {
    if (node.exportKind === 'type' || node.exportKind === 'typeof') {
      return makeChangeNode(true, false)(context, node);
    }
  },

  ExportAllDeclaration: function (context, node) {
    if (node.exportKind === 'type') {
      return makeChangeNode(true, false)(context, node);
    }
  },

  ImportDeclaration: function (context, node) {
    // if all import-spefiers in this declaration are for types, then remove the entire declaration
    if (allSpecifiersAreType(node)) {
      return makeChangeNode(false, false)(context, node);
    }

    // if this is a type-import, then remove the entire declaration
    if (nodeIsTypeImport(node)) {
      return makeChangeNode(false, false)(context, node);
    }
  },

  ImportSpecifier: function (context, node) {
    if (nodeIsTypeImport(node)) {
      var ast = context.ast;

      // Flow quirk: Remove importKind which is outside the node
      var idxStart = findTokenIndexAtStartOfNode(ast.tokens, node);
      var maybeImportKind = ast.tokens[idxStart - 1];
      var maybeImportKindLabel = getLabel(maybeImportKind);
      if (nodeIsTypeImport({ importKind: maybeImportKindLabel })) {
        makeChangeNode(true, false)(context, maybeImportKind);
      }

      makeChangeNode(false, false)(context, node);
      removeTrailingCommaNode(context, node);
      return false;
    }
  },

  ArrowFunctionExpression: function (context, node) {
    // Naively erasing a multi-line return type from an arrow function will
    // leave a newline between the parameter list and the arrow, which is not
    // valid JS. Detect this here and move the arrow up to the correct line.

    if (context.pretty) {
      // Pretty-printing solves this naturally. Good, because our arrow-fudging
      // below doesn't play nice with source maps... Which are only created when
      // using --pretty.
      return;
    }
    var returnType = node.returnType;
    if (returnType) {
      var ast = context.ast;
      var paramEndIdx = findTokenIndexAtStartOfNode(ast.tokens, returnType);
      do {
        paramEndIdx--;
      } while (isComment(ast.tokens[paramEndIdx]));

      var arrowIdx = findTokenIndexAtEndOfNode(ast.tokens, returnType);
      while (getLabel(ast.tokens[arrowIdx]) !== '=>') {
        arrowIdx++;
      }

      if (
        ast.tokens[paramEndIdx].loc.end.line <
        ast.tokens[arrowIdx].loc.start.line
      ) {
        // Insert an arrow immediately after the parameter list.
        makeChangeNode(false, false)(
          context,
          getSpliceNodeAtPos(
            context,
            endOf(ast.tokens[paramEndIdx]),
            ast.tokens[paramEndIdx].loc.end,
            ' =>',
          ),
        );

        // Delete the original arrow token.
        makeChangeNode(false, false)(context, ast.tokens[arrowIdx]);
      }
    }
  },
};

function removeInferredPredicateNode(context, node) {
  var tokens = context.ast.tokens;
  var priorTokenIdx = findTokenIndexAtStartOfNode(tokens, node) - 1;
  var token = tokens[priorTokenIdx];
  if (token && token.type === 'Punctuator' && token.value === ':') {
    makeChangeNode(false, false)(context, token);
  }
  return makeChangeNode(false, false)(context, node);
}

function removeTrailingCommaNode(context, node) {
  var ast = context.ast;

  // Remove trailing comma (potentially the next node)
  var idx = findTokenIndexAtEndOfNode(ast.tokens, node) + 1;

  while (isComment(ast.tokens[idx])) {
    // NOTE: ast.tokens has no comments in Flow
    idx++;
  }
  if (getLabel(ast.tokens[idx]) === ',') {
    makeChangeNode(false, false)(context, ast.tokens[idx]);
  }
  return false;
}

function getPragmaNode(context, start, size) {
  var source = context.source;
  var line = 1;
  var column = 0;
  for (var position = 0; position < start; position++) {
    var char = source[position];
    if (char === '\n') {
      line++;
      column = 0;
    } else if (char === '\r') {
      if (source[position + 1] === '\n') {
        position++;
      }
      line++;
      column = 0;
    } else {
      column++;
    }
  }
  return createNode({
    start: start,
    end: start + size,
    loc: {
      start: {line: line, column: column},
      end: {line: line, column: column + size},
    },
  });
}

function getLeadingSpaceNode(context, node) {
  var source = context.source;
  var end = startOf(node);
  var start = end;
  while (source[start - 1] === ' ' || source[start - 1] === '\t') {
    start--;
  }
  if (start !== end) {
    return createNode({
      start: start,
      end: end,
      loc: {start: node.loc.start, end: node.loc.start},
    });
  }
}

function getTrailingLineNode(context, node) {
  var source = context.source;
  var start = endOf(node);
  var end = start;
  while (source[end] === ' ' || source[end] === '\t') {
    end++;
  }

  // Remove all space including the line break if this token was alone on the line.
  if (source[end] === '\n' || source[end] === '\r') {
    if (source[end] === '\r' && source[end + 1] === '\n') {
      end++;
    }
    end++;

    if (isLastNodeRemovedFromLine(context, node)) {
      return createNode({
        start: start,
        end: end,
        loc: {start: node.loc.end, end: node.loc.end},
      });
    }
  }
}

// Creates a zero-width "node" with a value to splice at that position.
// WARNING: This is only safe to use when source maps are off!
function getSpliceNodeAtPos(context, pos, loc, value) {
  return createNode({
    start: pos,
    end: pos,
    loc: {start: loc, end: loc},
    __spliceValue: value,
  });
}

// Returns true if node is the last to be removed from a line.
function isLastNodeRemovedFromLine(context, node) {
  var tokens = context.ast.tokens;
  var priorTokenIdx = findTokenIndexAtStartOfNode(tokens, node) - 1;
  var token = tokens[priorTokenIdx];
  var line = node.loc.end.line;

  // Find previous token that was not removed on the same line.
  while (
    priorTokenIdx >= 0 &&
    token.loc.end.line === line &&
    isRemovedToken(context, token)
  ) {
    token = tokens[--priorTokenIdx];
  }

  // If there's no prior token (start of file), or the prior token is on another
  // line, this line must be fully removed.
  return !token || token.loc.end.line !== line;
}

// Returns true if the provided token was previously marked as removed.
function isRemovedToken(context, token) {
  var affectedNodes = context.affectedNodes;
  var nodeIdx = affectedNodes.length - 1;

  // Find the last removed node which could possibly contain this token.
  while (nodeIdx >= 0 && startOf(affectedNodes[nodeIdx].node) > startOf(token)) {
    nodeIdx--;
  }
  var opt = affectedNodes[nodeIdx];

  // This token couldn't be removed if not contained within the removed node.
  if (nodeIdx === -1 || endOf(opt.node) < endOf(token)) {
    return false;
  }

  var node = opt.node;

  // Iterate through the tokens contained by the removed node to find a match.
  var tokens = context.ast.tokens;
  var tokenIdx = findTokenIndexAtStartOfNode(tokens, node);
  while (endOf(tokens[tokenIdx]) <= endOf(node)) {
    if (token === tokens[tokenIdx]) {
      return true;
    }
    tokenIdx++;
  }

  return false;
}

// Given the AST output from the parser, walk through in a depth-first order,
// calling methods on the given visitor, providing context as the first argument.
function visit(ast, context, visitor) {
  var stack;
  var parent;
  var keys = [];
  var index = -1;

  do {
    index++;
    if (stack && index === keys.length) {
      parent = stack.parent;
      keys = stack.keys;
      index = stack.index;
      stack = stack.prev;
    } else {
      var node = parent ? parent[keys[index]] : getProgram(ast);
      if (node && typeof node === 'object' && (node.type || node.length)) {
        if (node.type) {
          var visitFn = visitor[node.type];
          if (visitFn && visitFn(context, node, ast) === false) {
            continue;
          }
        }
        stack = {parent: parent, keys: keys, index: index, prev: stack};
        parent = node;
        keys = Object.keys(node);
        index = -1;
      }
    }
  } while (stack);
}

// Given an array of sorted tokens, find the index of the token which contains
// the given offset. Uses binary search for O(log N) performance.
function findTokenIndex(tokens, offset, matchBiasParam) {
  // Token ranges often overlap at the edges e.g {start: 0, end: 100}, {start: 100, end: 300}. The
  // `matchBiasParam` param allows a user to specify what to do in this case, defaulting to matching
  // against the start of a token range.
  var matchBias = matchBiasParam === 'end' ? 'end' : 'start';
  var min = 0;
  var max = tokens.length - 1;
  var ptr;

  while (min <= max) {
    ptr = ((min + max) / 2) | 0;
    var token = tokens[ptr];
    var end = endOf(token);
    var start = startOf(token);
    if (end < offset || (matchBias === 'start' && end === offset)) {
      min = ptr + 1;
    } else if (start > offset || (matchBias === 'end' && start === offset)) {
      max = ptr - 1;
    } else {
      return ptr;
    }
  }

  return ptr;
}

function findTokenIndexAtStartOfNode(tokens, node) {
  return findTokenIndex(tokens, startOf(node), 'start');
}
function findTokenIndexAtEndOfNode(tokens, node) {
  return findTokenIndex(tokens, endOf(node), 'end');
}

// True if the provided token is a comment.
function isComment(token) {
  return token.type === 'Block' || token.type === 'Line';
}

// Produce a string full of space characters of a given size.
function space(size) {
  var sp = ' ';
  var result = '';

  for (;;) {
    if ((size & 1) === 1) {
      result += sp;
    }
    size >>>= 1;
    if (size === 0) {
      break;
    }
    sp += sp;
  }
  return result;
}

// Generate a source map when *removing* nodes rather than replacing them
// with spaces.
function generateSourceMappings(affectedNodes) {
  var mappings = '';
  if (!affectedNodes || affectedNodes.length === '') {
    return mappings;
  }

  var end = {line: 1, column: 0};

  for (var i = 0; i < affectedNodes.length; i++) {
    var start = affectedNodes[i].node.loc.start;
    var lineDiff = start.line - end.line;
    var columnDiff = start.column - end.column;
    if (lineDiff) {
      for (var l = 0; l !== lineDiff; l++) {
        mappings += ';';
      }
      mappings += vlq.encode([start.column, 0, lineDiff, columnDiff]);
    } else if (columnDiff) {
      if (i) {
        mappings += ',';
      }
      mappings += vlq.encode([columnDiff, 0, lineDiff, columnDiff]);
    }

    end = affectedNodes[i].node.loc.end;
    mappings += ',';
    mappings += vlq.encode([
      0,
      0,
      end.line - start.line,
      end.column - start.column,
    ]);
  }

  return mappings;
}

/**
 * A lightweight layer to abstract over the slightly different ASTs returned by
 * Flow vs Babylon.
 */

function startOf(token) {
  return token.range[0];
}

function endOf(token) {
  return token.range[1];
}

function getComments(ast) {
  return ast.comments;
}

function createNode(data) {
  return {
    range: [data.start, data.end],
    loc: data.loc,
    __spliceValue: data.__spliceValue,
  };
}

function getLabel(token) {
  return token.value;
}

function getProgram(ast) {
  return ast;
}
