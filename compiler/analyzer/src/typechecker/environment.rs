use errors::TypeErrorType;

use crate::{
    converge_types,
    utils::{get_implementation_of, get_method_types_from_symbol, symbol_to_type},
    EvaluatedType, ScopeId, SymbolIndex, SymbolLibrary,
};

/// An environment is a scoped area in which a type clause is assumed to be
/// satisfied. It is more or less a shadow of the symbol library.
#[derive(Debug)]
pub struct TypeEnvironment {
    pub id: ScopeId,
    pub suppositions: Vec<Supposition>,
}

/// A temporary benefit of doubt for a type constraint.
/// Each supposition is unique, in that it concerns a single generic.
#[derive(Debug)]
pub struct Supposition {
    /// The (most definitely) generic type that is being supposed against.
    pub base: SymbolIndex,
    /// The implementations on the type.
    pub implementations: Vec<EvaluatedType>,
    /// The derived type methods gotten by intersecting plausible implementations and typeforms.
    pub methods: Vec<SymbolIndex>,
}

impl Supposition {
    /// Creates a new supposition for a base generic type.
    /// It starts with an empty list of generics and methods.
    pub fn new(base: SymbolIndex) -> Self {
        Supposition {
            base,
            implementations: vec![],
            methods: vec![],
        }
    }
    /// Creates a new supposition by checking if an interface can be implemented can be made on a generic base.
    /// It can only be valid if if there are no previous conflicting implementations of the interface, either
    /// from previous type environments or the concrete generic truth.
    pub fn from_implementation(
        base: SymbolIndex,
        interface: EvaluatedType,
        symbollib: &SymbolLibrary,
    ) -> Result<Self, Vec<TypeErrorType>> {
        let eval_type = EvaluatedType::Generic { base };
        if let EvaluatedType::InterfaceInstance {
            interface_: interface_idx,
            ..
        } = &interface
        {
            if let Some(previous_interface_impl) =
                get_implementation_of(*interface_idx, &eval_type, symbollib)
            {
                if let None = converge_types(&interface, &previous_interface_impl, symbollib) {
                    let name = symbollib.format_evaluated_type(&eval_type);
                    let implementation_str = symbollib.format_evaluated_type(&interface);
                    let previous_implementation_str =
                        symbollib.format_evaluated_type(&previous_interface_impl);
                    return Err(vec![TypeErrorType::MismatchedImplementations {
                        name,
                        first: previous_implementation_str,
                        second: implementation_str,
                    }]);
                }
            }
        }
        // It would be a good idea to cross check the method list as well to see if there are methods with the same
        // name that diverge from methods in the interface, but this scenario is left as satisfiable, because:
        // - It won't affect the method ranking.
        // - There is no possible way of implementing an interface that conflicts with a method already defined,
        //   so the environment is basically unreachable.
        // - It is too much work for very little gain.
        let mut supposition = Supposition::new(base);
        supposition.implementations.push(interface);

        Ok(supposition)
    }
    /// Returns true if there are no interfaces or methods in the supposition.
    pub fn is_empty(&self) -> bool {
        self.methods.is_empty() && self.implementations.is_empty()
    }
    /// For two suppositions A and B, it represents the type clause `A and B`.
    ///
    /// It will only pass if there is no contradiction between the types of A and B.
    /// ## Note
    /// It must  be checked externally that both suppositions:
    /// - have the same base type.
    /// - do not contradict previously established type environments and concrete truths.
    pub fn and(
        self,
        other: Supposition,
        symbollib: &SymbolLibrary,
    ) -> Result<Supposition, Vec<TypeErrorType>> {
        // 1. for two suppositions A and B,  A ^ B can be supposed if A and B have absolutely no divergent methods.
        // We confirm this by aggregating the full list of method types for both suppositions,
        // selecting the method types with matching names, and asserting that they can converge.
        let methods_for_self = self.get_all_methods(symbollib);
        let methods_for_other = other.get_all_methods(symbollib);

        let matching_methods = methods_for_self
            .iter()
            .filter_map(|(method_name, method_type)| {
                methods_for_other
                    .iter()
                    .find(|(other_method_name, _)| other_method_name == method_name)
                    .map(|(_, other_method)| (method_name, method_type, other_method))
            });

        let mut mismatches = vec![];
        for (name, method_a, method_b) in matching_methods {
            if let None = converge_types(method_a, method_b, symbollib) {
                // todo: compare static and public.
                let method_a_as_str = symbollib.format_evaluated_type(method_a);
                let method_b_as_str = symbollib.format_evaluated_type(method_b);
                mismatches.push((*name, method_a_as_str, method_b_as_str))
            }
        }
        if !mismatches.is_empty() {
            if let Some(base_name) = symbollib.get(self.base).map(|symbol| symbol.name.as_str()) {
                return Err(mismatches
                    .into_iter()
                    .map(|(method_name, first_signature, second_signature)| {
                        let base_name = base_name.to_owned();
                        let method_name = method_name.to_owned();
                        TypeErrorType::MismatchedMethods {
                            base_name,
                            method_name,
                            first_signature,
                            second_signature,
                        }
                    })
                    .collect());
            }
        }
        // 2. For two suppositions A and B, A ^ B can be supposed if A and B have no conflicting implementations.
        // We confirm this by finding the implementations in both suppositions and asserting that they can converge.
        let matching_implementations = self.implementations.iter().filter_map(|implementation| {
            other
                .implementations
                .iter()
                .find(
                    |other_implementation| match (implementation, other_implementation) {
                        (
                            EvaluatedType::InterfaceInstance {
                                interface_: first, ..
                            },
                            EvaluatedType::InterfaceInstance {
                                interface_: second, ..
                            },
                        ) => first == second,
                        _ => false,
                    },
                )
                .map(|other_implementation| (implementation, other_implementation))
        });
        let mut mismatches = vec![];
        for (implementation_a, implementation_b) in matching_implementations {
            if let None = converge_types(implementation_a, implementation_b, symbollib) {
                if let Some(name) = symbollib.get(self.base).map(|symbol| symbol.name.as_str()) {
                    let name = name.to_owned();
                    let first = symbollib.format_evaluated_type(implementation_a);
                    let second = symbollib.format_evaluated_type(implementation_b);
                    mismatches.push(TypeErrorType::MismatchedImplementations {
                        name,
                        first,
                        second,
                    })
                }
            }
        }
        if !mismatches.is_empty() {
            return Err(mismatches);
        }
        // There could possibly be filtering out to deduplicate converged interfaces and methods,
        // but I find it to be too much work for very little gain.
        Ok(Supposition {
            base: self.base,
            implementations: self
                .implementations
                .into_iter()
                .chain(other.implementations.into_iter())
                .collect(),
            methods: self
                .methods
                .into_iter()
                .chain(other.methods.into_iter())
                .collect(),
        })
    }

    /// For two suppositions A and B, it represents the type clause `A or B`.
    ///
    /// It will pass by intersecting the suppositions, i.e. remove the methods and implementations
    /// that do not converge.
    /// ## Note
    /// It must be checked externally that both suppositions:
    /// - have the same base type.
    /// - do not contradict previously established type environments and concrete truths.
    pub fn or(
        self,
        other: Supposition,
        symbollib: &SymbolLibrary,
    ) -> Result<Supposition, Vec<TypeErrorType>> {
        todo!()
    }

    /// Returns all the methods in the supposition, both from the implementations and the intersected methods.
    ///
    /// _It does not return methods from the base or other type environments, only the supposition._
    pub fn get_all_methods<'a>(
        &self,
        symbollib: &'a SymbolLibrary,
    ) -> Vec<(&'a str, EvaluatedType)> {
        let mut method_types_for_self = self
            .methods
            .iter()
            .filter_map(|method_idx| {
                let symbol = symbollib.get(*method_idx)?;
                symbol_to_type(symbollib.get(*method_idx)?, *method_idx, symbollib)
                    .ok()
                    .map(|method_type| (symbol.name.as_str(), method_type))
            })
            .collect::<Vec<_>>();
        self.implementations.iter().for_each(|implementation| {
            if let EvaluatedType::InterfaceInstance {
                interface_,
                generic_arguments,
                ..
            } = implementation
            {
                get_method_types_from_symbol(*interface_, symbollib, generic_arguments)
                    .into_iter()
                    .for_each(|(method_name, method_type, _)| {
                        method_types_for_self.push((method_name.as_str(), method_type))
                    });
            };
        });
        method_types_for_self
    }
}
