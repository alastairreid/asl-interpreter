(****************************************************************
 * ASL let-hoisting transform
 *
 * Lifts let-bindings as high as possible out of expressions.
 *
 * The main restrictions on lifting let-bindings are
 *
 * - not lifting the let-binding out of a while guard condition
 *   in case the let-binding depends on a variable that is modified
 *   by the while loop or has side-effects.
 *
 * - not lifting the let-binding out of elsif conditions, case-alternative
 *   guards or catcher-guards in case the let-binding is evaluated
 *   earlier than other conditions/guards in the statement.
 *
 * - not lifting the let-binding out of the body of an if-expression
 *   or out of the second argument of && or || in case the let-binding
 *   contains a side-effect or can throw an exception or can
 *   trigger a runtime error.
 *
 * When these restrictions do not limit how high a binding can be lifted,
 * they normally turn into assignments prior to the statement that
 * contains the expression.
 *
 * Copyright (C) 2025-2025 Intel Corporation
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(****************************************************************
 * End
 ****************************************************************)
