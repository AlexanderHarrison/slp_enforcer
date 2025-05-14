use slp_parser::{Game, VectorI8};
use std::collections::HashSet;

pub type ViolationMask = u64;
pub mod violation {
    use super::ViolationMask;
    pub const TRAVEL_TIME       : ViolationMask = 1 << 0;
    pub const GOOMWAVE_CLAMPING : ViolationMask = 1 << 1;
    
    pub const ALL               : ViolationMask = TRAVEL_TIME | GOOMWAVE_CLAMPING;
    pub const RAW_COORD_REQUIRED: ViolationMask = ALL;
}

pub fn violation_name(mask: ViolationMask) -> &'static str {
    match mask {
        violation::TRAVEL_TIME => "Travel time",
        violation::GOOMWAVE_CLAMPING => "Goomwave clamping",
        _ => "Unknown",
    }
} 

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum StickType {
    Analog,
    Digital,
    Orca,
}

/*#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Controller {
    OEMOrPhob,
    Goomwave,
    Box,
    CubstrabtionOrCPad,
    Orca,
}*/

#[derive(Clone, Debug)]
pub struct PlayerViolations {
    /// Bitmask of which violations were detected for this player.
    pub found: ViolationMask,
    
    /// Whether the lstick is analog, digital, or orca.
    pub lstick_type: StickType,
    
    /*
        /// Whether the cstick is analog or digital.
        pub cstick_type: StickType,
        
        /// Guessed controller.
        pub controller: Controller,
    */
    
    /// The percentage of control stick movements that do not have intermediate values.
    ///
    /// Between 0 and 1.
    /// Values less than 0.25 will count as a violation. 
    pub travel_time_hit_rate: f32,
}

impl PlayerViolations {
    pub const DEFAULT: PlayerViolations = PlayerViolations {
        found: 0,
        lstick_type: StickType::Analog,
        
        travel_time_hit_rate: 0.0,
    };
}

#[derive(Clone, Debug)]
pub struct ViolationResult {
    /// Bitmask of which violations were checked.
    ///
    /// Some violations cannot be checked because the slp file is too old and
    /// does not contain the necessary information.
    ///
    /// Requirements:
    /// - Travel time: slp version >= 3.17.0
    pub checked: ViolationMask,
    
    /// Bitmask of which violations were skipped.
    pub skipped: ViolationMask,
    
    /// Violations for each player.
    ///
    /// If the player does not exist, then there will be no violations.
    pub players: [PlayerViolations; 4],
}

impl ViolationResult {
    pub const DEFAULT: ViolationResult = ViolationResult {
        checked: 0,
        skipped: 0,
        players: [
            PlayerViolations::DEFAULT,
            PlayerViolations::DEFAULT,
            PlayerViolations::DEFAULT,
            PlayerViolations::DEFAULT,
        ]
    };
}

pub fn iter_violations(mask: ViolationMask) -> impl Iterator<Item=ViolationMask> {
    let mut mask = mask;
    std::iter::from_fn(move || {
        let bit = mask.trailing_zeros();
        if bit == ViolationMask::BITS { return None; }
        
        let next = 1 << bit;
        mask ^= next;
        Some(next)
    })
}

pub fn check_game(game: &Game) -> ViolationResult {
    let mut res = ViolationResult::DEFAULT;
    
    if game.info.min_version(3, 17, 0) {
        res.checked |= violation::RAW_COORD_REQUIRED;
    } else {
        res.skipped |= violation::RAW_COORD_REQUIRED;
    }
    
    let mut stick_coords = Vec::new();
    let mut rim_coords = HashSet::<(i8, i8)>::with_capacity(4096);
    
    for ply in 0..4 {
        let Some(ref frames) = game.frames[ply] else { continue; };
        let vio = &mut res.players[ply];
        
        // push clamped lstick coords to buffer
        stick_coords.clear();
        stick_coords.reserve(frames.len());
        for f in frames {
            stick_coords.push(f.left_stick_coords_raw.clamped());
        }
        
        // determine digital/analog lstick
        vio.lstick_type = lstick_type(&mut rim_coords, &stick_coords);
    
        if vio.lstick_type == StickType::Digital {
            // digital lstick checks -------------------
        
            // travel time
            if res.checked & violation::TRAVEL_TIME != 0 {
                vio.travel_time_hit_rate = travel_time_hit_rate(&stick_coords);
                if vio.travel_time_hit_rate < 0.25 {
                    vio.found |= violation::TRAVEL_TIME;
                }
            }
        } else {
            // analog and orca lstick checks --------------------
            
            // goomwave clamping
            if res.checked & violation::GOOMWAVE_CLAMPING != 0 {
                if has_goomwave_clamping(&stick_coords) {
                    vio.found |= violation::GOOMWAVE_CLAMPING;
                }
            }
        }
    }
    
    res
}

pub fn lstick_type(
    rim_coords: &mut HashSet<(i8, i8)>,
    coords: &[VectorI8]
) -> StickType {
    rim_coords.clear();

    let mut nonrim_changes = 0;
    let mut nonrim_noncardinal_changes = 0;
    let mut rim_set = std::collections::HashSet::new();
    
    // square magnitude of rim. 80 is max ssbm coord distance.
    const RIM_MIN: f32 = 79.0*79.0;
    const RIM_COUNT: f32 = 432.0;
    
    for group in coords.windows(2) {
        let [a, b] = group else { panic!(); };
        
        let ax = a.x as f32;
        let ay = a.y as f32;
        let bx = b.x as f32;
        let by = b.y as f32;
        
        let a_on_rim = (ax*ax + ay*ay) > RIM_MIN;
        let b_on_rim = (bx*bx + by*by) > RIM_MIN;
        
        let ab_x = bx - ax;
        let ab_y = by - ay;
        
        if a_on_rim {
            rim_set.insert((a.x, a.y));
        }
        
        // if a ~= b or b ~= c then continue
        if ab_x.abs() + ab_y.abs() <= 2.0 { continue; }
        
        if a_on_rim && b_on_rim { continue; }
        
        let len = (ab_x*ab_x + ab_y*ab_y).sqrt();
        let nx = ab_x / len;
        let ny = ab_y / len;
        
        nonrim_changes += 1;
        if nx.abs() + ny.abs() > 1.01 { nonrim_noncardinal_changes += 1; } 
    }
    
    if nonrim_changes == 0 { return StickType::Digital; }
    let noncardinal_rate = nonrim_noncardinal_changes as f32 / nonrim_changes as f32;
    let rim_coverage = rim_set.len() as f32 / RIM_COUNT;
    
    // For shorter games rim coverage doesn't do the job, so lower coverage requirement.
    // altered from https://github.com/altf4/libenforcer/blob/9e8c3dabba8d42d30cbe513dfbe09c0e72bd989d/src/index.ts#L291
    const THREE_MINUTES: usize = 10800;
    let mut rim_coverage_required = 0.50;
    if coords.len() < THREE_MINUTES {
        rim_coverage_required /= (THREE_MINUTES - coords.len()) as f32 / THREE_MINUTES as f32 + 1.0;
    }
    
    dbg!(noncardinal_rate, rim_coverage, rim_coverage_required);
    
    if noncardinal_rate > 0.50 {
        StickType::Analog
    } else if rim_coverage > rim_coverage_required {
        StickType::Orca
    } else {
        StickType::Digital
    }
}

// altered from https://github.com/altf4/libenforcer/blob/9e8c3dabba8d42d30cbe513dfbe09c0e72bd989d/src/travel_time.ts#L20
fn travel_time_hit_rate(coords: &[VectorI8]) -> f32 {
    let mut travel_coord_count = 0;
    let mut target_count = 0;
    let mut last_coord = VectorI8 { x: 0, y: 0 };
    let mut is_target = true;
    let mut is_travel = false;
    
    for coord in coords.iter().copied() {
        if coord == last_coord {
            if !is_target {
                target_count += 1;
            }
            is_target = true;
            is_travel = false;
        } else {
            if !is_target && !is_travel {
                travel_coord_count += 1;
                is_travel = true;
            }
            is_target = false;
        }
        last_coord = coord;
    }
    
    if target_count <= 1 {
        return 1.0;
    }
    travel_coord_count as f32 / (target_count - 1) as f32
}

fn has_goomwave_clamping(coords: &[VectorI8]) -> bool {
    // stick values [-6, 6] are set to zero on goomwaves
    
    for coord in coords {
        if coord.x != 0 && -7 < coord.x && coord.x < 7 { return false; }
        if coord.y != 0 && -7 < coord.y && coord.y < 7 { return false; }
    }
    true
}

#[test]
fn test() {
    // p1_no_nerf_box ---------------------------------------
    // p1: box with travel time violation
    // p2: gcc with no violations
    
    let (game, _) = slp_parser::read_game(std::path::Path::new("test_slps/p1_no_nerf_box.slpz")).unwrap();
    let res = check_game(&game);
    
    assert_eq!(res.checked, violation::ALL);
    assert_eq!(res.skipped, 0);
    assert_eq!(res.players[0].found, violation::TRAVEL_TIME);
    assert_eq!(res.players[0].lstick_type, StickType::Digital);
    assert_eq!(res.players[1].found, 0);
    assert_eq!(res.players[1].lstick_type, StickType::Analog);
}
