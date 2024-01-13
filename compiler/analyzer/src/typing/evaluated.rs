use crate::{
    converge_types, unify_generic_arguments, unify_types,
    utils::{
        arrify, get_implementation_of, get_interface_types_from_symbol,
        get_method_types_from_symbol, maybify, symbol_to_type,
    },
    DiagnosticType, IntermediateType, IntermediateTypeClause, ParameterType, PathIndex,
    ProgramDiagnostic, SemanticSymbolKind, SymbolIndex, SymbolLibrary, TypecheckerContext,
    UnifyOptions, EVALUATION_DEPTH,
};
use ast::Span;
use errors::{
    TypeError,
    TypeErrorType,
    // TypeErrorType
};

/// A type expression, as is.
#[derive(Debug, PartialEq, Clone, Default)]
pub enum EvaluatedType {
    /// A type that evolves to different values, based on control flow.
    Partial {
        types: Vec<EvaluatedType>,
    },
    /// An instance created with `new A()`, or by labelling a value with `: A`.
    ModelInstance {
        model: SymbolIndex,
        is_invariant: bool,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    InterfaceInstance {
        interface_: SymbolIndex,
        is_invariant: bool,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    /// An instance of an enum created by assigning a variant.
    EnumInstance {
        enum_: SymbolIndex,
        is_invariant: bool,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    /// A named or anonymous function.
    FunctionInstance {
        function: SymbolIndex,
        is_invariant: bool,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    FunctionExpressionInstance {
        is_async: bool,
        is_invariant: bool,
        params: Vec<ParameterType>,
        return_type: Box<EvaluatedType>,
        generic_args: Vec<(SymbolIndex, EvaluatedType)>,
    },
    /// A method.
    MethodInstance {
        method: SymbolIndex,
        is_invariant: bool,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    /// A model value.
    Model(SymbolIndex),
    Interface(SymbolIndex),
    Enum(SymbolIndex),
    Module(SymbolIndex),
    OpaqueTypeInstance {
        available_methods: Vec<SymbolIndex>,
        available_interfaces: Vec<EvaluatedType>,
        aliased_as: Option<SymbolIndex>,
        collaborators: Vec<SymbolIndex>,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    Void,
    Never,
    #[default]
    Unknown,
    // Generics that cannot be mutated or coerced.
    HardGeneric {
        base: SymbolIndex,
    },
    Generic {
        base: SymbolIndex,
    },
}

impl EvaluatedType {
    /// Returns `true` if the evaluated type is [`Model`].
    ///
    /// [`Model`]: EvaluatedType::Model
    #[must_use]
    pub fn is_model(&self) -> bool {
        matches!(self, Self::Model(..))
    }

    /// Returns `true` if the evaluated type is [`Interface`].
    ///
    /// [`Interface`]: EvaluatedType::Interface
    #[must_use]
    pub fn is_interface(&self) -> bool {
        matches!(self, Self::Interface(..))
    }

    /// Returns `true` if the evaluated type is [`Enum`].
    ///
    /// [`Enum`]: EvaluatedType::Enum
    #[must_use]
    pub fn is_enum(&self) -> bool {
        matches!(self, Self::Enum(..))
    }

    /// Returns `true` if the evaluated type is [`Module`].
    ///
    /// [`Module`]: EvaluatedType::Module
    #[must_use]
    pub fn is_module(&self) -> bool {
        matches!(self, Self::Module(..))
    }

    /// Returns `true` if the evaluated type is [`ModelInstance`].
    ///
    /// [`ModelInstance`]: EvaluatedType::ModelInstance
    #[must_use]
    pub fn is_model_instance(&self) -> bool {
        matches!(self, Self::ModelInstance { .. })
    }
    /// Returns `true` if the evaluated type is [`Void`].
    ///
    /// [`Void`]: EvaluatedType::Void
    #[must_use]
    pub fn is_void(&self) -> bool {
        matches!(self, Self::Void)
    }

    /// Returns `true` if the evaluated type is [`Unknown`].
    ///
    /// [`Unknown`]: EvaluatedType::Unknown
    #[must_use]
    pub fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }

    /// Returns `true` if the evaluated type is [`InterfaceInstance`].
    ///
    /// [`InterfaceInstance`]: EvaluatedType::InterfaceInstance
    #[must_use]
    pub fn is_interface_instance(&self) -> bool {
        matches!(self, Self::InterfaceInstance { .. })
    }

    /// Returns `true` if the evaluated type is [`Generic`].
    ///
    /// [`Generic`]: EvaluatedType::Generic
    #[must_use]
    pub fn is_generic(&self) -> bool {
        matches!(self, Self::Generic { .. } | Self::HardGeneric { .. })
    }

    /// Returns `true` if the evaluated type is [`Partial`].
    ///
    /// [`Partial`]: EvaluatedType::Partial
    #[must_use]
    pub fn is_partial(&self) -> bool {
        matches!(self, Self::Partial { .. })
    }

    /// Returns true if a certain type is an inclusive child within this.
    pub fn contains(&self, child: &EvaluatedType) -> bool {
        self == child
            || match self {
                EvaluatedType::Partial { types } => types.iter().any(|typ| typ.contains(child)),
                EvaluatedType::ModelInstance {
                    generic_arguments, ..
                }
                | EvaluatedType::InterfaceInstance {
                    generic_arguments, ..
                }
                | EvaluatedType::EnumInstance {
                    generic_arguments, ..
                }
                | EvaluatedType::FunctionInstance {
                    generic_arguments, ..
                }
                | EvaluatedType::MethodInstance {
                    generic_arguments, ..
                } => generic_arguments
                    .iter()
                    .map(|(_, typ)| typ)
                    .any(|typ| typ.contains(child)),
                EvaluatedType::FunctionExpressionInstance {
                    params,
                    return_type,
                    generic_args,
                    ..
                } => {
                    params
                        .iter()
                        .any(|param| param.inferred_type.contains(child))
                        || return_type.contains(child)
                        || generic_args
                            .iter()
                            .map(|(_, typ)| typ)
                            .any(|typ| typ.contains(child))
                }
                EvaluatedType::OpaqueTypeInstance { .. } => false,
                _ => false,
            }
    }

    /// Returns `true` if the evaluated type is [`Never`].
    ///
    /// [`Never`]: EvaluatedType::Never
    #[must_use]
    pub fn is_never(&self) -> bool {
        matches!(self, Self::Never)
    }

    /// Gather generics in an evaluated type into a tuple map.
    pub fn gather_generics_into(&self, generic_map: &mut Vec<SymbolIndex>) {
        match self {
            EvaluatedType::Partial { types } => types
                .iter()
                .for_each(|typ| typ.gather_generics_into(generic_map)),
            EvaluatedType::ModelInstance {
                generic_arguments, ..
            }
            | EvaluatedType::InterfaceInstance {
                generic_arguments, ..
            }
            | EvaluatedType::EnumInstance {
                generic_arguments, ..
            }
            | EvaluatedType::FunctionInstance {
                generic_arguments, ..
            }
            | EvaluatedType::FunctionExpressionInstance {
                generic_args: generic_arguments,
                ..
            }
            | EvaluatedType::MethodInstance {
                generic_arguments, ..
            } => generic_arguments
                .iter()
                .for_each(|(_, eval)| eval.gather_generics_into(generic_map)),
            EvaluatedType::HardGeneric { base, .. } | EvaluatedType::Generic { base } => {
                generic_map.push(*base)
            }
            _ => {}
        }
    }

    /// Returns `true` if the evaluated type is [`MethodInstance`].
    ///
    /// [`MethodInstance`]: EvaluatedType::MethodInstance
    #[must_use]
    pub fn is_method_instance(&self) -> bool {
        matches!(self, Self::MethodInstance { .. })
    }

    /// Returns true if, contained within this type is another type that matches a given predicate.
    /// The check is inclusive, meaning it will return true if this type matches the predicate.
    pub fn contains_child_for_which(&self, predicate: &impl Fn(&EvaluatedType) -> bool) -> bool {
        predicate(self)
            || match self {
                EvaluatedType::Partial { types } => types
                    .iter()
                    .any(|typ| typ.contains_child_for_which(predicate)),
                EvaluatedType::ModelInstance {
                    generic_arguments, ..
                }
                | EvaluatedType::InterfaceInstance {
                    generic_arguments, ..
                }
                | EvaluatedType::EnumInstance {
                    generic_arguments, ..
                }
                | EvaluatedType::FunctionInstance {
                    generic_arguments, ..
                }
                | EvaluatedType::MethodInstance {
                    generic_arguments, ..
                } => generic_arguments
                    .iter()
                    .map(|(_, typ)| typ)
                    .any(|typ| typ.contains_child_for_which(predicate)),
                EvaluatedType::FunctionExpressionInstance {
                    params,
                    return_type,
                    generic_args,
                    ..
                } => {
                    params
                        .iter()
                        .any(|param| param.inferred_type.contains_child_for_which(predicate))
                        || return_type.contains_child_for_which(predicate)
                        || generic_args
                            .iter()
                            .map(|(_, typ)| typ)
                            .any(|typ| typ.contains_child_for_which(predicate))
                }
                EvaluatedType::OpaqueTypeInstance { .. } => false,
                _ => false,
            }
    }

    pub(crate) fn is_invariant(&self) -> bool {
        match self {
            EvaluatedType::Partial { types } => types.iter().all(|typ| typ.is_invariant()),
            EvaluatedType::InterfaceInstance { is_invariant, .. }
            | EvaluatedType::EnumInstance { is_invariant, .. }
            | EvaluatedType::FunctionInstance { is_invariant, .. }
            | EvaluatedType::FunctionExpressionInstance { is_invariant, .. }
            | EvaluatedType::MethodInstance { is_invariant, .. }
            | EvaluatedType::ModelInstance { is_invariant, .. } => *is_invariant,
            EvaluatedType::HardGeneric { .. } => true,
            _ => false,
        }
    }

    pub(crate) fn set_invariance(&mut self, value: bool) {
        match self {
            EvaluatedType::Partial { types } => {
                types.iter_mut().for_each(|typ| typ.set_invariance(value))
            }
            EvaluatedType::InterfaceInstance { is_invariant, .. }
            | EvaluatedType::EnumInstance { is_invariant, .. }
            | EvaluatedType::FunctionInstance { is_invariant, .. }
            | EvaluatedType::MethodInstance { is_invariant, .. }
            | EvaluatedType::ModelInstance { is_invariant, .. } => *is_invariant = value,
            EvaluatedType::FunctionExpressionInstance {
                is_invariant,
                generic_args,
                params,
                return_type,
                ..
            } => {
                *is_invariant = value;
                generic_args
                    .iter_mut()
                    .for_each(|(_, typ)| typ.set_invariance(value));
                params
                    .iter_mut()
                    .for_each(|param| param.inferred_type.set_invariance(value));
                return_type.set_invariance(value);
            }
            _ => {}
        }
    }

    pub(crate) fn is_soft_generic(&self) -> bool {
        matches!(self, EvaluatedType::Generic { .. })
    }

    /// Traverses an evalutated type.
    pub(crate) fn traverse(&self, predicate: &mut impl FnMut(&EvaluatedType)) {
        predicate(self);
        match self {
            EvaluatedType::Partial { types } => {
                types.iter().for_each(|typ| typ.traverse(predicate))
            }
            EvaluatedType::ModelInstance {
                generic_arguments, ..
            }
            | EvaluatedType::InterfaceInstance {
                generic_arguments, ..
            }
            | EvaluatedType::EnumInstance {
                generic_arguments, ..
            }
            | EvaluatedType::FunctionInstance {
                generic_arguments, ..
            }
            | EvaluatedType::MethodInstance {
                generic_arguments, ..
            } => generic_arguments
                .iter()
                .map(|(_, typ)| typ)
                .for_each(|typ| typ.traverse(predicate)),
            EvaluatedType::FunctionExpressionInstance {
                params,
                return_type,
                generic_args,
                ..
            } => {
                params
                    .iter()
                    .for_each(|param| param.inferred_type.traverse(predicate));
                return_type.traverse(predicate);
                generic_args
                    .iter()
                    .map(|(_, typ)| typ)
                    .for_each(|typ| typ.traverse(predicate))
            }
            _ => {}
        }
    }

    /// Returns `true` if the evaluated type is [`EnumInstance`].
    ///
    /// [`EnumInstance`]: EvaluatedType::EnumInstance
    #[must_use]
    pub fn is_enum_instance(&self) -> bool {
        matches!(self, Self::EnumInstance { .. })
    }

    /// Returns true if a type can be used in concrete type contexts.
    pub fn is_concrete(&self) -> bool {
        matches!(
            self,
            EvaluatedType::HardGeneric { .. }
                | EvaluatedType::OpaqueTypeInstance { .. }
                | EvaluatedType::Generic { .. }
                | EvaluatedType::EnumInstance { .. }
                | EvaluatedType::ModelInstance { .. }
                | EvaluatedType::Never
        )
    }

    /// Returns `true` if the evaluated type is [`FunctionExpressionInstance`].
    ///
    /// [`FunctionExpressionInstance`]: EvaluatedType::FunctionExpressionInstance
    #[must_use]
    pub fn is_function_expression_instance(&self) -> bool {
        matches!(self, Self::FunctionExpressionInstance { .. })
    }
}

/// Converts an intermediate type into an evaluation.
pub fn evaluate(
    typ: &IntermediateType,
    symbollib: &SymbolLibrary,
    // A map of the solutions for previously encountered generic types.
    solved_generics: Option<&Vec<(SymbolIndex, EvaluatedType)>>,
    // Error store from the standpoint, if it exists.
    mut error_tracker: &mut Option<(&mut Vec<ProgramDiagnostic>, PathIndex, Span)>,
    // A value that safe guards against infinitely recursive union types, or indirect recursion in type aliases.
    mut recursion_depth: u64,
) -> EvaluatedType {
    if recursion_depth == EVALUATION_DEPTH {
        return EvaluatedType::Never;
    } else {
        recursion_depth += 1;
    }
    match typ {
        // Dependent types should only be allowed in method returns and
        // implementation names, so they cannot be evaluated directly.
        IntermediateType::BoundConstraintType { .. } => {
            add_error_if_possible(error_tracker, TypeErrorType::IllegalBoundConstraintType);
            return EvaluatedType::Unknown;
        }
        IntermediateType::FunctionType {
            params,
            return_type,
            ..
        } => evaluate_function_type(
            return_type,
            symbollib,
            solved_generics,
            error_tracker,
            recursion_depth,
            params,
        ),
        // Accessing a member type of another module.
        IntermediateType::MemberType {
            object, property, ..
        } => match evaluate_member_type(
            object,
            symbollib,
            solved_generics,
            error_tracker,
            recursion_depth,
            property,
        ) {
            Ok(value) => value,
            Err(value) => return value,
        },
        IntermediateType::SimpleType {
            value,
            generic_args,
            ..
        } => {
            let idx = symbollib.forward(*value);
            let typ = match symbollib.get_forwarded(idx) {
                Some(value) => value,
                None => return EvaluatedType::Unknown,
            };
            let mut get_generics = |generic_params| {
                generate_generics_from_arguments(
                    generic_args,
                    generic_params,
                    error_tracker,
                    typ,
                    symbollib,
                    solved_generics,
                    recursion_depth,
                )
            };
            match &typ.kind {
                SemanticSymbolKind::Module { .. } => EvaluatedType::Module(idx),
                SemanticSymbolKind::Interface { generic_params, .. } => {
                    EvaluatedType::InterfaceInstance {
                        interface_: idx,
                        generic_arguments: get_generics(generic_params),
                        is_invariant: false,
                    }
                }
                SemanticSymbolKind::Model { generic_params, .. } => EvaluatedType::ModelInstance {
                    model: idx,
                    generic_arguments: get_generics(generic_params),
                    is_invariant: false,
                },
                SemanticSymbolKind::Enum { generic_params, .. } => EvaluatedType::EnumInstance {
                    enum_: idx,
                    generic_arguments: get_generics(generic_params),
                    is_invariant: false,
                },
                SemanticSymbolKind::TypeName {
                    generic_params,
                    value,
                    ..
                } => {
                    let generic_arguments = get_generics(generic_params);
                    let mut value_typ = evaluate(
                        value,
                        symbollib,
                        Some(&generic_arguments),
                        error_tracker,
                        recursion_depth,
                    );
                    if let EvaluatedType::OpaqueTypeInstance { aliased_as, .. } = &mut value_typ {
                        *aliased_as = Some(idx);
                    };
                    value_typ
                }
                SemanticSymbolKind::GenericParameter { .. } => {
                    // check if this type already has a solution.
                    if let Some(solved_generics) = solved_generics {
                        for (generic_parameter, evaluated_type) in solved_generics {
                            if *generic_parameter == idx {
                                return (*evaluated_type).clone();
                            }
                        }
                    }
                    EvaluatedType::HardGeneric { base: idx }
                }
                SemanticSymbolKind::UndeclaredValue => EvaluatedType::Unknown,
                SemanticSymbolKind::Import { source: None, .. } => EvaluatedType::Unknown,
                _ => {
                    if error_tracker.is_some() {
                        let old = error_tracker.as_ref().unwrap().1;
                        error_tracker.as_mut().unwrap().1 =
                            typ.references.first().unwrap().module_path;
                        add_error_if_possible(
                            &mut error_tracker,
                            TypeErrorType::ValueAsType {
                                name: typ.name.clone(),
                            },
                        );
                        error_tracker.as_mut().unwrap().1 = old;
                    }
                    EvaluatedType::Unknown
                }
            }
        }
        IntermediateType::This { meaning, span } => {
            // Generate a type that is equal to the enclosing model or interface.
            match meaning.clone() {
                Some(value) => {
                    let symbol = symbollib.get_forwarded(value).unwrap();
                    // Pertaining to the `This` type, interfaces can be solved as a unique type of generics.
                    // In the implementing model, `This` should refer to the model/generic itself, not the interface.
                    if let Some(prior_generics) = solved_generics.as_ref() {
                        if let Some(solution) = prior_generics.iter().find(|tuple| tuple.0 == value)
                        {
                            return solution.1.clone();
                        }
                    }
                    let generic_args = match &symbol.kind {
                        SemanticSymbolKind::Model { generic_params, .. }
                        | SemanticSymbolKind::Interface { generic_params, .. } => generic_params
                            .iter()
                            .map(|idx| IntermediateType::SimpleType {
                                value: *idx,
                                generic_args: vec![],
                                span: Span::default(),
                            })
                            .collect(),

                        _ => vec![],
                    };
                    let intermediate_type = IntermediateType::SimpleType {
                        value,
                        generic_args,
                        span: *span,
                    };
                    evaluate(
                        &intermediate_type,
                        symbollib,
                        solved_generics,
                        error_tracker,
                        recursion_depth,
                    )
                }
                None => EvaluatedType::Unknown,
            }
        }
        IntermediateType::UnionType { types, .. } => {
            let mut collaborators = vec![];
            let mut generic_arguments = vec![];
            let mut available_methods = vec![];
            let mut available_interfaces: Vec<EvaluatedType> = vec![];
            for typ in types {
                let eval_type = evaluate(
                    typ,
                    symbollib,
                    solved_generics,
                    error_tracker,
                    recursion_depth,
                );
                match eval_type {
                    EvaluatedType::ModelInstance {
                        model: collab,
                        generic_arguments: collab_generics,
                        ..
                    }
                    | EvaluatedType::EnumInstance {
                        enum_: collab,
                        generic_arguments: collab_generics,
                        ..
                    } => {
                        let unification = unify_generic_arguments(
                            &generic_arguments,
                            &collab_generics,
                            symbollib,
                            UnifyOptions::None,
                            None,
                        )
                        .ok();
                        if let Some(final_args) = unification {
                            generic_arguments = final_args;
                            collaborators.push(collab);
                        }
                    }
                    EvaluatedType::OpaqueTypeInstance {
                        collaborators: mut subcollaborators,
                        ..
                    } => {
                        collaborators.append(&mut subcollaborators);
                        continue;
                    }
                    EvaluatedType::Generic { base } | EvaluatedType::HardGeneric { base, .. } => {
                        collaborators.push(base)
                    }
                    // if any of the united types evaluates to a never,
                    // then all types are corrupted, and the final result
                    // is a never type.
                    EvaluatedType::Never => return EvaluatedType::Never,
                    _ => continue,
                };
            }
            // Gathers the methods and interfaces that are general to every collaborator.
            // It starts by assuming every method and interface in the first collaborator is available
            // And then uses the rest to filter through..
            for (index, _collaborator) in collaborators.iter().enumerate() {
                let methods_for_this_collaborator =
                    get_method_types_from_symbol(*_collaborator, symbollib, &generic_arguments);
                let interfaces_for_this_collaborator =
                    get_interface_types_from_symbol(*_collaborator, symbollib, &generic_arguments);
                if index == 0 {
                    available_methods = methods_for_this_collaborator;
                    available_interfaces = interfaces_for_this_collaborator;
                } else {
                    available_methods.retain(|(method_name, method_type, is_public)| {
                        methods_for_this_collaborator.iter().any(
                            |(collab_method_name, collab_method_type, collab_method_is_public)| {
                                collab_method_name == method_name
                                    && is_public == collab_method_is_public
                                    && unify_types(
                                        method_type,
                                        collab_method_type,
                                        symbollib,
                                        UnifyOptions::None,
                                        None,
                                    )
                                    .is_ok()
                            },
                        )
                    });
                    available_interfaces.retain(|interface_type| {
                        interfaces_for_this_collaborator
                            .iter()
                            .any(|collab_interface_type| {
                                match (&collab_interface_type, interface_type) {
                                    (
                                        EvaluatedType::InterfaceInstance {
                                            interface_: first,
                                            generic_arguments: left_generics,
                                            ..
                                        },
                                        EvaluatedType::InterfaceInstance {
                                            interface_: second,
                                            generic_arguments: right_generics,
                                            ..
                                        },
                                    ) => {
                                        first == second
                                            && unify_generic_arguments(
                                                left_generics,
                                                right_generics,
                                                symbollib,
                                                UnifyOptions::None,
                                                None,
                                            )
                                            .is_ok()
                                    }
                                    _ => false,
                                }
                            })
                    });
                }
            }
            let available_methods = available_methods
                .into_iter()
                .filter_map(|(_, eval_typ, _)| match eval_typ {
                    EvaluatedType::MethodInstance { method, .. } => Some(method),
                    _ => None,
                })
                .collect::<Vec<_>>();
            return EvaluatedType::OpaqueTypeInstance {
                collaborators,
                generic_arguments,
                available_methods,
                available_interfaces,
                aliased_as: None,
            };
        }
        IntermediateType::Placeholder => return EvaluatedType::Unknown,
        IntermediateType::ArrayType { element_type, .. } => {
            if symbollib.array.is_some() {
                return arrify(
                    evaluate(
                        &element_type,
                        symbollib,
                        solved_generics,
                        error_tracker,
                        recursion_depth,
                    ),
                    symbollib,
                );
            }
            add_error_if_possible(
                error_tracker,
                TypeErrorType::MissingIntrinsic {
                    name: format!("Array"),
                },
            );
            return EvaluatedType::Unknown;
        }
        IntermediateType::MaybeType { value, .. } => {
            if symbollib.maybe.is_some() {
                return maybify(
                    evaluate(
                        &value,
                        symbollib,
                        solved_generics,
                        error_tracker,
                        recursion_depth,
                    ),
                    symbollib,
                );
            }
            add_error_if_possible(
                error_tracker,
                TypeErrorType::MissingIntrinsic {
                    name: format!("Maybe"),
                },
            );
            return EvaluatedType::Unknown;
        }
        IntermediateType::TernaryType {
            clause,
            consequent,
            alternate,
            ..
        } => evaluate_ternary_type(
            symbollib,
            solved_generics,
            error_tracker,
            recursion_depth,
            clause,
            consequent,
            alternate,
        ),
    }
}

/// Computes a ternary type into a single evaluation.
/// Type evaluation is eager, meaning that it will compute both branches
/// before determining the one to return.
fn evaluate_ternary_type(
    symbollib: &SymbolLibrary,
    solved_generics: Option<&Vec<(SymbolIndex, EvaluatedType)>>,
    error_tracker: &mut Option<(&mut Vec<ProgramDiagnostic>, PathIndex, Span)>,
    recursion_depth: u64,
    clause: &Box<IntermediateTypeClause>,
    consequent: &Box<IntermediateType>,
    alternate: &Box<IntermediateType>,
) -> EvaluatedType {
    let evaluated_consequent = evaluate(
        &consequent,
        symbollib,
        solved_generics,
        error_tracker,
        recursion_depth,
    );
    let evaluated_alternate = evaluate(
        &alternate,
        symbollib,
        solved_generics,
        error_tracker,
        recursion_depth,
    );
    let verity = evaluate_type_clause(
        clause,
        symbollib,
        solved_generics,
        error_tracker,
        recursion_depth,
    );
    match verity {
        Some(true) => evaluated_consequent,
        Some(false) => evaluated_alternate,
        None => EvaluatedType::Unknown,
    }
}

/// Evaluates an intermediate member type into an evaluated type.
fn evaluate_member_type(
    object: &Box<IntermediateType>,
    symbollib: &SymbolLibrary,
    solved_generics: Option<&Vec<(SymbolIndex, EvaluatedType)>>,
    error_tracker: &mut Option<(&mut Vec<ProgramDiagnostic>, PathIndex, Span)>,
    recursion_depth: u64,
    property: &crate::IntermediateTypeProperty,
) -> Result<EvaluatedType, EvaluatedType> {
    let object_type = evaluate(
        &*object,
        symbollib,
        solved_generics,
        error_tracker,
        recursion_depth,
    );
    match &object_type {
        EvaluatedType::Module(module) => {
            let module_symbol = match symbollib.get(*module) {
                Some(symbol) => symbol,
                None => return Err(EvaluatedType::Unknown),
            };
            let base_type = || module_symbol.name.clone();
            let property_name = || property.name.clone();
            if let SemanticSymbolKind::Module {
                global_declaration_symbols,
                ..
            } = &module_symbol.kind
            {
                for symbol_idx in global_declaration_symbols {
                    let global_symbol = ast::unwrap_or_continue!(symbollib.get(*symbol_idx));
                    if global_symbol.name == property.name {
                        if !global_symbol.kind.is_public() {
                            add_error_if_possible(error_tracker, {
                                TypeErrorType::NonPublicType {
                                    base_type: base_type(),
                                    property: property_name(),
                                }
                            })
                        }
                        // Create an intermeduate type to evaluate.
                        let intermediate = IntermediateType::SimpleType {
                            value: *symbol_idx,
                            generic_args: property.generic_args.clone(),
                            span: property.span,
                        };
                        return Err(evaluate(
                            &intermediate,
                            symbollib,
                            solved_generics,
                            error_tracker,
                            recursion_depth,
                        ));
                    }
                }
                // No match.
                add_error_if_possible(
                    error_tracker,
                    TypeErrorType::NoSuchProperty {
                        base_type: base_type(),
                        property: property_name(),
                    },
                );
                return Err(EvaluatedType::Unknown);
            }
        }
        EvaluatedType::Unknown => return Err(EvaluatedType::Unknown),
        _ => {}
    }
    add_error_if_possible(
        error_tracker,
        TypeErrorType::NotAModuleType {
            object_type: symbollib.format_evaluated_type(&object_type),
        },
    );
    return Err(EvaluatedType::Unknown);
}

/// Evaluates an intermediate function type into an evaluated type.
fn evaluate_function_type(
    return_type: &Option<Box<IntermediateType>>,
    symbollib: &SymbolLibrary,
    solved_generics: Option<&Vec<(SymbolIndex, EvaluatedType)>>,
    mut error_tracker: &mut Option<(&mut Vec<ProgramDiagnostic>, PathIndex, Span)>,
    recursion_depth: u64,
    params: &Vec<ParameterType>,
) -> EvaluatedType {
    let return_type = Box::new(
        return_type
            .as_ref()
            .map(|typ| {
                evaluate(
                    typ,
                    symbollib,
                    solved_generics,
                    error_tracker,
                    recursion_depth,
                )
            })
            .unwrap_or(EvaluatedType::Void),
    );
    EvaluatedType::FunctionExpressionInstance {
        is_async: false, // todo: always false (for now).
        params: params
            .iter()
            .map(|param| {
                let mut param = param.clone();
                param.inferred_type = param
                    .type_label
                    .as_ref()
                    .map(|typ| {
                        evaluate(
                            typ,
                            symbollib,
                            solved_generics,
                            &mut error_tracker,
                            recursion_depth,
                        )
                    })
                    .unwrap_or(EvaluatedType::Unknown);
                param
            })
            .collect(),
        is_invariant: false,
        return_type,
        generic_args: solved_generics.cloned().unwrap_or(vec![]),
    }
}

/// Computes a type clause and returns whether it is true or not.
pub fn evaluate_type_clause(
    clause: &IntermediateTypeClause,
    symbollib: &SymbolLibrary,
    solved_generics: Option<&Vec<(SymbolIndex, EvaluatedType)>>,
    error_tracker: &mut Option<(&mut Vec<ProgramDiagnostic>, PathIndex, Span)>,
    recursion_depth: u64,
) -> Option<bool> {
    match clause {
        // e.g. A is B and C implements D
        IntermediateTypeClause::Binary {
            left,
            operator,
            right,
        } => evaluate_binary_type_clause(
            left,
            right,
            operator,
            symbollib,
            solved_generics,
            error_tracker,
            recursion_depth,
        ),
        IntermediateTypeClause::Is { base, other } => {
            let symbol = symbollib.get(*base)?;
            let lhs = match symbol_to_type(symbol, *base, symbollib) {
                Ok(evaluated_type) => evaluated_type,
                Err(error_type) => {
                    add_error_if_possible(error_tracker, error_type);
                    return None;
                }
            };
            let rhs = evaluate(
                other,
                symbollib,
                solved_generics,
                error_tracker,
                recursion_depth,
            );
            return converge_types(lhs, rhs, symbollib).map(|_| true);
        }
        IntermediateTypeClause::Implements { base, interfaces } => {
            // The actual base type being checked.
            let lhs = IntermediateType::SimpleType {
                value: *base,
                generic_args: vec![],
                span: Span::default(),
            };
            let lhs = evaluate(
                &lhs,
                symbollib,
                solved_generics,
                error_tracker,
                recursion_depth,
            );
            // For every interface in the clause, check if it is implemented as is in the left hand type.
            for intermediate_type in interfaces {
                let evaled_interface = evaluate(
                    intermediate_type,
                    symbollib,
                    solved_generics,
                    error_tracker,
                    recursion_depth,
                );
                if let EvaluatedType::InterfaceInstance { interface_, .. } = &evaled_interface {
                    let actual_impl_type = get_implementation_of(*interface_, &lhs, symbollib)?;
                    converge_types(evaled_interface, actual_impl_type, symbollib)?;
                } else {
                    let asstr = symbollib.format_evaluated_type(&evaled_interface);
                    add_error_if_possible(
                        error_tracker,
                        TypeErrorType::ExpectedInterface { got: asstr },
                    );
                    return None;
                }
            }
            return Some(true);
        }
    }
}

fn evaluate_binary_type_clause(
    left: &Box<IntermediateTypeClause>,
    right: &Box<IntermediateTypeClause>,
    operator: &ast::LogicOperator,
    symbollib: &SymbolLibrary,
    solved_generics: Option<&Vec<(SymbolIndex, EvaluatedType)>>,
    error_tracker: &mut Option<(&mut Vec<ProgramDiagnostic>, PathIndex, Span)>,
    recursion_depth: u64,
) -> Option<bool> {
    let left = evaluate_type_clause(
        left,
        symbollib,
        solved_generics,
        error_tracker,
        recursion_depth,
    )?;
    let right = evaluate_type_clause(
        right,
        symbollib,
        solved_generics,
        error_tracker,
        recursion_depth,
    )?;
    match operator {
        ast::LogicOperator::And | ast::LogicOperator::AndLiteral => Some(left && right),
        ast::LogicOperator::Or | ast::LogicOperator::OrLiteral => Some(left || right),
    }
}

/// Converts a set of parameter indexes into their correct inferred type.
pub fn evaluate_parameter_idxs(
    params: &Vec<SymbolIndex>,
    symbollib: &SymbolLibrary,
    generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    checker_ctx: &mut TypecheckerContext<'_>,
) -> Vec<(SymbolIndex, EvaluatedType)> {
    let mut evaluated_param_types = vec![];
    for param in params {
        let parameter_symbol = symbollib.get(*param).unwrap();
        let inferred_type = match &parameter_symbol.kind {
            SemanticSymbolKind::Parameter { param_type, .. } => {
                if let Some(declared_type) = param_type {
                    let span = parameter_symbol.ident_span();
                    evaluate(
                        declared_type,
                        symbollib,
                        Some(&generic_arguments),
                        &mut checker_ctx.tracker(span),
                        0,
                    )
                } else {
                    EvaluatedType::Unknown
                }
            }
            _ => EvaluatedType::Unknown,
        };
        evaluated_param_types.push((*param, inferred_type));
    }
    evaluated_param_types
}

/// A variant of evaluate() that extracts consequents from bounded constraint types.
pub fn evaluate_and_ignore_constraint(
    typ: &IntermediateType,
    symbollib: &SymbolLibrary,
    // A map of the solutions for previously encountered generic types.
    solved_generics: Option<&Vec<(SymbolIndex, EvaluatedType)>>,
    // Error store from the standpoint, if it exists.
    error_tracker: &mut Option<(&mut Vec<ProgramDiagnostic>, PathIndex, Span)>,
    // A value that safe guards against infinitely recursive union types, or indirect recursion in type aliases.
    recursion_depth: u64,
) -> EvaluatedType {
    match typ {
        IntermediateType::BoundConstraintType { consequent, .. } => evaluate(
            &consequent,
            symbollib,
            solved_generics,
            error_tracker,
            recursion_depth,
        ),
        _ => evaluate(
            typ,
            symbollib,
            solved_generics,
            error_tracker,
            recursion_depth,
        ),
    }
}

fn generate_generics_from_arguments(
    generic_args: &Vec<IntermediateType>,
    generic_params: &Vec<SymbolIndex>,
    mut error_tracker: &mut Option<(&mut Vec<ProgramDiagnostic>, PathIndex, Span)>,
    typ: &crate::SemanticSymbol,
    symbollib: &SymbolLibrary,
    solved_generics: Option<&Vec<(SymbolIndex, EvaluatedType)>>,
    recursion_depth: u64,
) -> Vec<(SymbolIndex, EvaluatedType)> {
    if generic_args.len() > 0 && generic_params.len() == 0 {
        add_error_if_possible(
            &mut error_tracker,
            TypeErrorType::UnexpectedGenericArgs {
                name: typ.name.clone(),
            },
        );
        vec![]
    } else {
        let args_len = generic_args.len();
        let param_len = generic_params.len();

        let mut generic_solutions = vec![];
        // Unify each parameter to argument.
        let mut i = 0;
        while i < param_len {
            // Convert each parameter symbol to an immediately evaluated generic param type.
            // which can then be unified with the argument
            // to determine the result value.
            let param_idx = generic_params[i];
            let intermediate_generic_type = IntermediateType::SimpleType {
                value: param_idx,
                generic_args: vec![],
                span: Span::default(),
            };
            let generic_param_evaluated = evaluate(
                &intermediate_generic_type,
                symbollib,
                solved_generics,
                &mut error_tracker,
                recursion_depth,
            );
            // If there is no generic argument at the current position, but the parameter has a default value,
            // use that instead, else the matching is insufficient.
            let mut generic_argument_intermediate = generic_args.get(i);
            if generic_argument_intermediate.is_none() {
                if let Some(SemanticSymbolKind::GenericParameter {
                    default_value: Some(default_value),
                    ..
                }) = symbollib.get(param_idx).map(|symbol| &symbol.kind)
                {
                    generic_argument_intermediate = Some(default_value);
                } else {
                    add_error_if_possible(&mut error_tracker, {
                        let name = typ.name.clone();
                        TypeErrorType::MismatchedGenericArgs {
                            name,
                            expected: param_len,
                            assigned: args_len,
                        }
                    });
                    return generic_solutions;
                }
            }

            let argument_evaluated = evaluate(
                &generic_argument_intermediate.unwrap(),
                symbollib,
                solved_generics,
                &mut error_tracker,
                recursion_depth,
            );
            let result_evaluated_type = match unify_types(
                &generic_param_evaluated,
                &argument_evaluated,
                symbollib,
                UnifyOptions::HardConform,
                None,
            ) {
                Ok(value) => value,
                Err(error) => {
                    for error in error {
                        add_error_if_possible(&mut error_tracker, error);
                    }
                    EvaluatedType::Unknown
                }
            };
            generic_solutions.push((param_idx, result_evaluated_type));
            i += 1;
        }
        if generic_args.len() > generic_params.len() {
            add_error_if_possible(&mut error_tracker, {
                let name = typ.name.clone();
                TypeErrorType::MismatchedGenericArgs {
                    name,
                    expected: param_len,
                    assigned: args_len,
                }
            });
        }
        generic_solutions
    }
}

fn add_error_if_possible(
    error_tracker: &mut Option<(&mut Vec<ProgramDiagnostic>, PathIndex, Span)>,
    error_type: TypeErrorType,
) {
    if let Some((errors, path_idx, span)) = error_tracker {
        let error = ProgramDiagnostic {
            offending_file: *path_idx,
            _type: DiagnosticType::Error(crate::Error::Typing({
                TypeError {
                    _type: error_type,
                    span: *span,
                }
            })),
        };
        if !errors.iter().any(|prior| *prior == error) {
            errors.push(error);
        }
    }
}
