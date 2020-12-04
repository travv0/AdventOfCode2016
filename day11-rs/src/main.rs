use itertools::Itertools;
use std::cmp::Ordering;
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

pub fn main() {
    let mut state = State::new();
    state.floors[0].insert(Object {
        element: "strontium",
        kind: Kind::Generator,
    });
    state.floors[0].insert(Object {
        element: "strontium",
        kind: Kind::Microchip,
    });
    state.floors[0].insert(Object {
        element: "plutonium",
        kind: Kind::Generator,
    });
    state.floors[0].insert(Object {
        element: "plutonium",
        kind: Kind::Microchip,
    });
    state.floors[1].insert(Object {
        element: "thulium",
        kind: Kind::Generator,
    });
    state.floors[1].insert(Object {
        element: "ruthenium",
        kind: Kind::Generator,
    });
    state.floors[1].insert(Object {
        element: "ruthenium",
        kind: Kind::Microchip,
    });
    state.floors[1].insert(Object {
        element: "curium",
        kind: Kind::Generator,
    });
    state.floors[1].insert(Object {
        element: "curium",
        kind: Kind::Microchip,
    });
    state.floors[2].insert(Object {
        element: "thulium",
        kind: Kind::Microchip,
    });
    {
        let (depth, nodes) = num_of_steps_to_complete(&state);
        println!("Part 1: {} (nodes checked: {})", depth.unwrap(), nodes);
    }

    state.floors[0].insert(Object {
        element: "elerium",
        kind: Kind::Generator,
    });
    state.floors[0].insert(Object {
        element: "elerium",
        kind: Kind::Microchip,
    });
    state.floors[0].insert(Object {
        element: "dilithium",
        kind: Kind::Generator,
    });
    state.floors[0].insert(Object {
        element: "dilithium",
        kind: Kind::Microchip,
    });
    {
        let (depth, nodes) = num_of_steps_to_complete(&state);
        println!("Part 2: {} (nodes checked: {})", depth.unwrap(), nodes);
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug, PartialOrd, Ord)]
enum Kind {
    Generator,
    Microchip,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug, PartialOrd, Ord)]
struct Object<'a> {
    element: &'a str,
    kind: Kind,
}

#[derive(Eq, Clone, Debug)]
struct State<'a> {
    step: usize,
    floors: [HashSet<Object<'a>>; 4],
    elevator_floor: usize,
}

impl<'a> State<'a> {
    fn new() -> Self {
        let floors = [
            HashSet::new(),
            HashSet::new(),
            HashSet::new(),
            HashSet::new(),
        ];
        Self {
            step: 0,
            floors,
            elevator_floor: 0,
        }
    }

    fn is_valid(&self) -> bool {
        for floor in &self.floors {
            for object in floor {
                if object.kind == Kind::Microchip
                    && !floor.contains(&Object {
                        kind: Kind::Generator,
                        element: object.element,
                    })
                    && floor.len() > 1
                {
                    for other in floor {
                        if other.kind == Kind::Generator {
                            return false;
                        }
                    }
                }
            }
        }
        true
    }

    fn is_final(&self) -> bool {
        for (i, floor) in self.floors.iter().enumerate() {
            if i != self.floors.len() - 1 && !floor.is_empty() {
                return false;
            }
        }
        true
    }

    fn next_steps(&self) -> Vec<Self> {
        let mut result = vec![];
        let floor = &self.floors[self.elevator_floor];
        let mut objects = vec![];

        for object in floor {
            objects.push(object);
        }

        let mut combos = vec![];
        for &i in &[1, 2] {
            if i <= objects.len() {
                let mut cs = objects.iter().combinations(i).collect();
                combos.append(&mut cs);
            }
        }

        for combo in combos {
            let dirs = [-1, 1];
            for &dir in &dirs {
                if dir == -1 && 0 < self.elevator_floor
                    || dir == 1 && self.elevator_floor < self.floors.len() - 1
                {
                    let mut new_state = Self::new();
                    new_state.step = self.step + 1;
                    new_state.elevator_floor = (self.elevator_floor as isize + dir) as usize;
                    new_state.floors = self.floors.clone();
                    for object in &combo {
                        new_state.floors[self.elevator_floor].remove(object);
                        new_state.floors[new_state.elevator_floor].insert(***object);
                    }
                    if new_state.is_valid() {
                        result.push(new_state)
                    }
                }
            }
        }

        result
    }

    fn heuristic(&self) -> usize {
        self.floors
            .iter()
            .enumerate()
            .map(|(i, floor)| (self.floors.len() - i - 1) * floor.len())
            .sum()
    }
}

#[test]
fn test_is_valid() {
    {
        let mut state = State::new();
        state.floors[0].insert(Object {
            element: "hydrogen",
            kind: Kind::Microchip,
        });
        state.floors[0].insert(Object {
            element: "lithium",
            kind: Kind::Microchip,
        });
        state.floors[1].insert(Object {
            element: "hydrogen",
            kind: Kind::Generator,
        });
        state.floors[2].insert(Object {
            element: "lithium",
            kind: Kind::Generator,
        });
        assert!(state.is_valid());
    }
    {
        let mut state = State::new();
        state.floors[2].insert(Object {
            element: "hydrogen",
            kind: Kind::Microchip,
        });
        state.floors[0].insert(Object {
            element: "lithium",
            kind: Kind::Microchip,
        });
        state.floors[1].insert(Object {
            element: "hydrogen",
            kind: Kind::Generator,
        });
        state.floors[2].insert(Object {
            element: "lithium",
            kind: Kind::Generator,
        });
        assert!(!state.is_valid());
    }
}

#[test]
fn test_is_final() {
    {
        let mut state = State::new();
        state.floors[3].insert(Object {
            element: "hydrogen",
            kind: Kind::Microchip,
        });
        state.floors[3].insert(Object {
            element: "lithium",
            kind: Kind::Microchip,
        });
        state.floors[3].insert(Object {
            element: "hydrogen",
            kind: Kind::Generator,
        });
        state.floors[3].insert(Object {
            element: "lithium",
            kind: Kind::Generator,
        });
        assert!(state.is_final());
    }
    {
        let mut state = State::new();
        state.floors[3].insert(Object {
            element: "hydrogen",
            kind: Kind::Microchip,
        });
        state.floors[3].insert(Object {
            element: "lithium",
            kind: Kind::Microchip,
        });
        state.floors[3].insert(Object {
            element: "hydrogen",
            kind: Kind::Generator,
        });
        state.floors[2].insert(Object {
            element: "lithium",
            kind: Kind::Generator,
        });
        assert!(!state.is_final());
    }
}

impl<'a> Hash for State<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.elevator_floor.hash(state);
        for (i, floor) in self.floors.iter().enumerate() {
            let mut vec = floor.iter().collect::<Vec<_>>();
            vec.sort();
            for obj in vec {
                obj.kind.hash(state);
                i.hash(state);
            }
        }
    }
}

impl<'a> PartialEq for State<'a> {
    fn eq(&self, other: &Self) -> bool {
        if self.elevator_floor == other.elevator_floor {
            let mut pairs = HashMap::new();
            for (i, floor) in self.floors.iter().enumerate() {
                for Object { element, .. } in floor {
                    let v = pairs.entry(element).or_insert(vec![]);
                    v.push(i);
                }
            }
            let mut other_pairs = HashMap::new();
            for (i, floor) in self.floors.iter().enumerate() {
                for Object { element, .. } in floor {
                    let v = other_pairs.entry(element).or_insert(vec![]);
                    v.push(i);
                }
            }
            for (_, pair) in pairs.iter_mut() {
                pair.sort_unstable();
                let pair_in_other_pairs = other_pairs
                    .values_mut()
                    .map(|other_pair| {
                        other_pair.sort_unstable();
                        other_pair
                    })
                    .any(|other_pair| other_pair == pair);
                if !pair_in_other_pairs {
                    return false;
                }
            }
            return true;
        }
        false
    }
}

impl<'a> PartialOrd for State<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.step.partial_cmp(&other.step)
    }
}

impl<'a> Ord for State<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.step.cmp(&other.step)
    }
}

fn num_of_steps_to_complete(state: &State) -> (Option<usize>, usize) {
    let state = Rc::new(state.clone());
    let mut open_set = BinaryHeap::new();
    let mut discovered = HashSet::new();

    let mut g_scores = HashMap::new();
    g_scores.insert(Rc::clone(&state), 0);

    let mut f_scores = HashMap::new();
    f_scores.insert(Rc::clone(&state), state.heuristic());

    discovered.insert(state.clone());
    open_set.push(Reverse(Rc::clone(&state)));

    while let Some(Reverse(current)) = open_set.pop() {
        if current.is_final() {
            return (Some(current.step), discovered.len());
        }

        for neighbor in current.next_steps() {
            let neighbor = Rc::new(neighbor);
            let tentative_g_scores = *g_scores
                .get(&current)
                .expect("current node without g score")
                + 1;
            if tentative_g_scores < *g_scores.entry(Rc::clone(&neighbor)).or_insert(usize::MAX) {
                // let last_len = g_scores.len();
                g_scores.insert(Rc::clone(&neighbor), tentative_g_scores);
                // if last_len != g_scores.len() {
                // println!("{} {}", last_len, g_scores.len());
                // }
                f_scores.insert(Rc::clone(&neighbor), neighbor.heuristic());
                if !discovered.contains(&neighbor) {
                    open_set.push(Reverse(Rc::clone(&neighbor)));
                    discovered.insert(Rc::clone(&neighbor));
                }
            }
        }
    }
    (None, discovered.len())
}

#[test]
fn test_num_of_steps_to_complete() {
    let mut state = State::new();
    state.floors[0].insert(Object {
        element: "hydrogen",
        kind: Kind::Microchip,
    });
    state.floors[0].insert(Object {
        element: "lithium",
        kind: Kind::Microchip,
    });
    state.floors[1].insert(Object {
        element: "hydrogen",
        kind: Kind::Generator,
    });
    state.floors[2].insert(Object {
        element: "lithium",
        kind: Kind::Generator,
    });

    assert_eq!(Some(11), num_of_steps_to_complete(&state).0);
}
