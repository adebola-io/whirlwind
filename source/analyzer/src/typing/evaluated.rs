use crate::{
    unify_generic_arguments, unify_types,
    utils::{get_method_types_from_symbol, get_trait_types_from_symbol},
    IntermediateType, ParameterType, PathIndex, ProgramError, SemanticSymbolKind, SymbolIndex,
    SymbolTable, UnifyOptions,
};
use ast::Span;
use errors::{
    value_as_type,
    TypeError,
    // TypeErrorType
};

/// A type expression, as is.
#[derive(Debug, PartialEq, Clone)]
pub enum EvaluatedType {
    /// A type that evolves to different values, based on control flow.
    Partial {
        types: Vec<EvaluatedType>,
    },
    /// An instance created with `new A()`, or by labelling a value with `: A`.
    ModelInstance {
        model: SymbolIndex,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    TraitInstance {
        trait_: SymbolIndex,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    /// An instance of an enum created by assigning a variant.
    EnumInstance {
        enum_: SymbolIndex,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    /// A named or anonymous function.
    FunctionInstance {
        function: SymbolIndex,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    FunctionExpressionInstance {
        is_async: bool,
        params: Vec<ParameterType>,
        return_type: Box<EvaluatedType>,
        generic_args: Vec<(SymbolIndex, EvaluatedType)>,
    },
    /// A method.
    MethodInstance {
        method: SymbolIndex,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    /// A model value.
    Model(SymbolIndex),
    Trait(SymbolIndex),
    Enum(SymbolIndex),
    Module(SymbolIndex),
    OpaqueTypeInstance {
        available_methods: Vec<SymbolIndex>,
        available_traits: Vec<EvaluatedType>,
        aliased_as: Option<SymbolIndex>,
        collaborators: Vec<SymbolIndex>,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    Void,
    Never,
    Unknown,
    // Generics that cannot be mutated or coerced.
    HardGeneric {
        base: SymbolIndex,
    },
    Generic {
        base: SymbolIndex,
    },
    Borrowed {
        base: Box<EvaluatedType>,
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

    /// Returns `true` if the evaluated type is [`Trait`].
    ///
    /// [`Trait`]: EvaluatedType::Trait
    #[must_use]
    pub fn is_trait(&self) -> bool {
        matches!(self, Self::Trait(..))
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

    /// Returns `true` if the evaluated type is [`TraitInstance`].
    ///
    /// [`TraitInstance`]: EvaluatedType::TraitInstance
    #[must_use]
    pub fn is_trait_instance(&self) -> bool {
        matches!(self, Self::TraitInstance { .. })
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
                | EvaluatedType::TraitInstance {
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
                EvaluatedType::Borrowed { base } => base.contains(child),
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
            | EvaluatedType::TraitInstance {
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
            EvaluatedType::HardGeneric { base } | EvaluatedType::Generic { base } => {
                generic_map.push(*base)
            }
            EvaluatedType::Borrowed { base } => base.gather_generics_into(generic_map),
            _ => {}
        }
    }

    /// Returns `true` if the evaluated type is [`Borrowed`].
    ///
    /// [`Borrowed`]: EvaluatedType::Borrowed
    #[must_use]
    pub fn is_borrowed(&self) -> bool {
        matches!(self, Self::Borrowed { .. })
    }

    /// Returns `true` if the evaluated type is [`MethodInstance`].
    ///
    /// [`MethodInstance`]: EvaluatedType::MethodInstance
    #[must_use]
    pub fn is_method_instance(&self) -> bool {
        matches!(self, Self::MethodInstance { .. })
    }
}

/// Converts an intermediate type into an evaluation.
pub fn evaluate(
    typ: &IntermediateType,
    symboltable: &SymbolTable,
    // A map of the solutions for previously encountered generic types.
    solved_generics: Option<&Vec<(SymbolIndex, EvaluatedType)>>,
    // Error store from the standpoint, if it exists.
    mut error_tracker: &mut Option<(&mut Vec<ProgramError>, PathIndex)>,
    // A value that safe guards against infinitely recursive union types, or indirect recursion in type aliases.
    mut recursion_depth: u64,
) -> EvaluatedType {
    if recursion_depth == 2000 {
        return EvaluatedType::Never;
    } else {
        recursion_depth += 1;
    }
    let evaltype = match typ {
        IntermediateType::FunctionType {
            params,
            return_type,
            ..
        } => {
            let return_type = Box::new(
                return_type
                    .as_ref()
                    .map(|typ| {
                        evaluate(
                            typ,
                            symboltable,
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
                                    symboltable,
                                    solved_generics,
                                    &mut error_tracker,
                                    recursion_depth,
                                )
                            })
                            .unwrap_or(EvaluatedType::Unknown);
                        param
                    })
                    .collect(),
                return_type,
                generic_args: solved_generics.cloned().unwrap_or(vec![]),
            }
        }
        IntermediateType::MemberType { .. } => EvaluatedType::Unknown, // todo
        IntermediateType::SimpleType {
            value,
            generic_args,
            span,
        } => {
            let idx = symboltable.forward(*value);
            let typ = symboltable.get_forwarded(idx).unwrap();
            let mut get_generics = |generic_params| {
                generate_generics_from_arguments(
                    generic_args,
                    generic_params,
                    error_tracker,
                    typ,
                    span,
                    symboltable,
                    solved_generics,
                )
            };
            match &typ.kind {
                SemanticSymbolKind::Module { .. } => EvaluatedType::Module(idx),
                SemanticSymbolKind::Trait { generic_params, .. } => EvaluatedType::TraitInstance {
                    trait_: idx,
                    generic_arguments: get_generics(generic_params),
                },
                SemanticSymbolKind::Model { generic_params, .. } => EvaluatedType::ModelInstance {
                    model: idx,
                    generic_arguments: get_generics(generic_params),
                },
                SemanticSymbolKind::Enum { generic_params, .. } => EvaluatedType::EnumInstance {
                    enum_: idx,
                    generic_arguments: get_generics(generic_params),
                },
                SemanticSymbolKind::TypeName {
                    generic_params,
                    value,
                    ..
                } => {
                    let generic_arguments = get_generics(generic_params);
                    let mut value_typ = evaluate(
                        value,
                        symboltable,
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
                _ => {
                    if error_tracker.is_some() {
                        let old = error_tracker.as_ref().unwrap().1;
                        error_tracker.as_mut().unwrap().1 =
                            typ.references.first().unwrap().module_path;
                        add_error_if_possible(
                            &mut error_tracker,
                            value_as_type(typ.name.clone(), *span),
                        );
                        error_tracker.as_mut().unwrap().1 = old;
                    }
                    EvaluatedType::Unknown
                }
            }
        }
        IntermediateType::This { meaning, span } => {
            // Generate a type that is equal to the enclosing model or trait.
            match meaning.clone() {
                Some(value) => {
                    let symbol = symboltable.get_forwarded(value).unwrap();
                    // Pertaining to the `This` type, traits can be solved as a unique type of generics.
                    // In the implementing model, `This` should refer to the model itself, not the trait.
                    if let Some(prior_generics) = solved_generics.as_ref() {
                        if let Some(solution) = prior_generics.iter().find(|tuple| tuple.0 == value)
                        {
                            return solution.1.clone();
                        }
                    }
                    let generic_args = match &symbol.kind {
                        SemanticSymbolKind::Model { generic_params, .. }
                        | SemanticSymbolKind::Trait { generic_params, .. } => generic_params
                            .iter()
                            .map(|idx| IntermediateType::SimpleType {
                                value: *idx,
                                generic_args: vec![],
                                span: Span::default(),
                            })
                            .collect(),

                        _ => {
                            return EvaluatedType::Unknown;
                        }
                    };
                    let intermediate_type = IntermediateType::SimpleType {
                        value,
                        generic_args,
                        span: *span,
                    };
                    evaluate(
                        &intermediate_type,
                        symboltable,
                        solved_generics,
                        error_tracker,
                        recursion_depth,
                    )
                }
                None => EvaluatedType::Unknown,
            }
        }
        IntermediateType::BorrowedType { value, .. } => EvaluatedType::Borrowed {
            base: Box::new(evaluate(
                &value,
                symboltable,
                solved_generics,
                error_tracker,
                recursion_depth,
            )),
        },
        IntermediateType::UnionType { types, .. } => {
            let mut collaborators = vec![];
            let mut generic_arguments = vec![];
            let mut available_methods = vec![];
            let mut available_traits: Vec<EvaluatedType> = vec![];
            for typ in types {
                let eval_type = evaluate(
                    typ,
                    symboltable,
                    solved_generics,
                    error_tracker,
                    recursion_depth,
                );
                match eval_type {
                    EvaluatedType::ModelInstance {
                        model: collab,
                        generic_arguments: collab_generics,
                    }
                    | EvaluatedType::EnumInstance {
                        enum_: collab,
                        generic_arguments: collab_generics,
                    } => {
                        let unification = unify_generic_arguments(
                            &generic_arguments,
                            &collab_generics,
                            symboltable,
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
                    EvaluatedType::Generic { base } | EvaluatedType::HardGeneric { base } => {
                        collaborators.push(base)
                    }
                    // if any of the united types evaluates to a never,
                    // then all types are corrupted, and the final result
                    // is a never type.
                    EvaluatedType::Never => return EvaluatedType::Never,
                    EvaluatedType::Borrowed { .. } => continue, // todo. How will this be handled?
                    _ => continue,
                };
            }
            // Gathers the methods and traits that are general to every collaborator.
            // It starts by assuming every method and trait in the first collaborator is available
            // And then uses the rest to filter through..
            for (index, _collaborator) in collaborators.iter().enumerate() {
                let methods_for_this_collaborator =
                    get_method_types_from_symbol(*_collaborator, symboltable, &generic_arguments);
                let traits_for_this_collaborator =
                    get_trait_types_from_symbol(*_collaborator, symboltable, &generic_arguments);
                if index == 0 {
                    available_methods = methods_for_this_collaborator;
                    available_traits = traits_for_this_collaborator;
                } else {
                    available_methods.retain(|(method_name, method_type, is_public)| {
                        methods_for_this_collaborator.iter().any(
                            |(collab_method_name, collab_method_type, collab_method_is_public)| {
                                collab_method_name == method_name
                                    && is_public == collab_method_is_public
                                    && unify_types(
                                        method_type,
                                        collab_method_type,
                                        symboltable,
                                        UnifyOptions::None,
                                        None,
                                    )
                                    .is_ok()
                            },
                        )
                    });
                    available_traits.retain(|trait_type| {
                        traits_for_this_collaborator
                            .iter()
                            .any(|collab_trait_type| match (&collab_trait_type, trait_type) {
                                (
                                    EvaluatedType::TraitInstance {
                                        trait_: first,
                                        generic_arguments: left_generics,
                                    },
                                    EvaluatedType::TraitInstance {
                                        trait_: second,
                                        generic_arguments: right_generics,
                                    },
                                ) => {
                                    first == second
                                        && unify_generic_arguments(
                                            left_generics,
                                            right_generics,
                                            symboltable,
                                            UnifyOptions::None,
                                            None,
                                        )
                                        .is_ok()
                                }
                                _ => false,
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
                available_traits,
                aliased_as: None,
            };
        }
        _ => EvaluatedType::Unknown,
    };
    evaltype
}

fn generate_generics_from_arguments(
    generic_args: &Vec<IntermediateType>,
    generic_params: &Vec<SymbolIndex>,
    mut error_tracker: &mut Option<(&mut Vec<ProgramError>, PathIndex)>,
    typ: &crate::SemanticSymbol,
    span: &Span,
    symboltable: &SymbolTable,
    solved_generics: Option<&Vec<(SymbolIndex, EvaluatedType)>>,
) -> Vec<(SymbolIndex, EvaluatedType)> {
    if generic_args.len() > 0 && generic_params.len() == 0 {
        add_error_if_possible(
            &mut error_tracker,
            errors::unexpected_generic_args(typ.name.clone(), *span),
        );
        vec![]
    } else {
        let args_len = generic_args.len();
        let param_len = generic_params.len();
        if args_len != param_len {
            add_error_if_possible(
                &mut error_tracker,
                errors::mismatched_generics(typ.name.clone(), param_len, args_len, *span),
            );
            vec![]
        } else {
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
                    symboltable,
                    solved_generics,
                    &mut error_tracker,
                    0,
                );
                let argument_evaluated = evaluate(
                    &generic_args[i],
                    symboltable,
                    solved_generics,
                    &mut error_tracker,
                    0,
                );
                let result_evaluated_type = match unify_types(
                    &generic_param_evaluated,
                    &argument_evaluated,
                    symboltable,
                    UnifyOptions::HardConform,
                    None,
                ) {
                    Ok(value) => value,
                    Err(error) => {
                        for error in error {
                            add_error_if_possible(
                                &mut error_tracker,
                                TypeError {
                                    _type: error,
                                    span: generic_args[i].span(),
                                },
                            );
                        }
                        EvaluatedType::Unknown
                    }
                };
                generic_solutions.push((param_idx, result_evaluated_type));
                i += 1;
            }
            generic_solutions
        }
    }
}

fn add_error_if_possible(
    error_tracker: &mut Option<(&mut Vec<ProgramError>, PathIndex)>,
    error: TypeError,
) {
    if let Some((errors, path_idx)) = error_tracker {
        let error = ProgramError {
            offending_file: *path_idx,
            error_type: crate::ProgramErrorType::Typing(error),
        };
        if !errors.iter().any(|prior| *prior == error) {
            errors.push(error);
        }
    }
}
