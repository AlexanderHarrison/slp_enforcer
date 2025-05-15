use slp_parser::{Game, VectorI8};
use std::collections::HashSet;

const RIM_MIN: f32 = 79.0*79.0;
const RIM_MIN_I: isize = 79*79;

pub type ViolationMask = u64;
pub mod violation {
    use super::ViolationMask;
    pub const NO_TRAVEL_TIME        : ViolationMask = 1 << 0;
    pub const NO_FUZZING            : ViolationMask = 1 << 1;
    pub const ENLARGED_DEADZONES    : ViolationMask = 1 << 2;
    pub const UTILT_ROUNDING        : ViolationMask = 1 << 3;
    pub const ILLEGAL_LSTICK_COORDS : ViolationMask = 1 << 4;
    pub const ILLEGAL_CSTICK_COORDS : ViolationMask = 1 << 5;
}

pub mod violation_group {
    use super::violation::*;
    use super::ViolationMask;
    
    pub const ALL: ViolationMask = NO_TRAVEL_TIME | NO_FUZZING 
        | ENLARGED_DEADZONES | UTILT_ROUNDING
        | ILLEGAL_LSTICK_COORDS | ILLEGAL_CSTICK_COORDS;
        
    pub const RAW_COORD_REQUIRED: ViolationMask = ALL;
    pub const GOOMWAVE: ViolationMask = ENLARGED_DEADZONES | UTILT_ROUNDING;
    
    pub const LSTICK_CHECKS_DIGITAL : ViolationMask = NO_TRAVEL_TIME | NO_FUZZING | ILLEGAL_LSTICK_COORDS;
    pub const LSTICK_CHECKS_ANALOG  : ViolationMask = ENLARGED_DEADZONES | UTILT_ROUNDING;
    pub const LSTICK_CHECKS_ORCA    : ViolationMask = LSTICK_CHECKS_ANALOG;
    
    pub const CSTICK_CHECKS_DIGITAL : ViolationMask = ILLEGAL_CSTICK_COORDS;
    pub const CSTICK_CHECKS_ANALOG  : ViolationMask = 0;
    pub const CSTICK_CHECKS_ORCA    : ViolationMask = CSTICK_CHECKS_DIGITAL;
    
    //pub const BOX_CHECKS: ViolationMask = ;
}

pub const fn violation_name(mask: ViolationMask) -> &'static str {
    match mask {
        violation::NO_TRAVEL_TIME => "No travel time",
        violation::NO_FUZZING => "No coordinate fuzzing",
        violation::ENLARGED_DEADZONES => "Enlarged deadzones",
        violation::UTILT_ROUNDING => "Up tilt rounding",
        _ => "Unknown",
    }
}
 
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum StickType {
    Analog,
    Digital,
    Orca,
    Unknown,
}

impl StickType {
    pub const fn name(self) -> &'static str {
        match self {
            StickType::Analog => "Analog",
            StickType::Digital => "Digital",
            StickType::Orca => "Orca",
            StickType::Unknown => "Unknown",
        }
    }

    pub const fn lstick_checks(self) -> ViolationMask {
        match self {
            StickType::Digital => violation_group::LSTICK_CHECKS_DIGITAL,
            StickType::Analog | StickType::Orca => violation_group::LSTICK_CHECKS_ANALOG,
            StickType::Unknown => 0,
        }
    }
    
    pub const fn cstick_checks(self) -> ViolationMask {
        match self {
            StickType::Digital => violation_group::CSTICK_CHECKS_DIGITAL,
            
            // Note that the orca controller has a digial cstick, NOT an orca style cstick.
            StickType::Analog | StickType::Orca => violation_group::CSTICK_CHECKS_ANALOG,
            
            StickType::Unknown => 0,
        }
    }
}

impl std::fmt::Display for StickType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

impl std::fmt::Display for Controller {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Controller {
    GCC,
    Goomwave,
    Box,
    
    /// This is a broad category, including:
    /// - GCC with cpad
    /// - Box with nunchuck
    /// - Fightstick
    /// - Cubstraption
    AnalogLStickDigitalCStick,
    
    Orca,
    Unknown,
}

impl Controller {
    pub const fn guess(
        lstick: StickType,
        cstick: StickType,
        violations: ViolationMask,
    ) -> Controller {
        if violations & violation_group::GOOMWAVE != 0 {
            return Controller::Goomwave;
        }
        
        use StickType::*;
        match (lstick, cstick) {
            (Analog, Analog) => Controller::GCC,
            (Digital, Digital) => Controller::Box,
            (Analog, Digital) => Controller::AnalogLStickDigitalCStick,
            (Orca, Digital) => Controller::Orca,
            _ => Controller::Unknown,
        }
    }
    
    pub const fn checks(self) -> ViolationMask {
        self.lstick().lstick_checks()
            | self.cstick().cstick_checks()
    }
    
    pub const fn name(self) -> &'static str {
        match self {
            Controller::GCC                       => "GCC",
            Controller::Goomwave                  => "Goomwave",
            Controller::Box                       => "Box",
            Controller::AnalogLStickDigitalCStick => "Cubstraption, Nunchuck B0XX, Fightstick, or CPad",
            Controller::Orca                      => "Orca",
            Controller::Unknown                   => "Unknown",
        }
    }
    
    pub const fn lstick(self) -> StickType {
        match self {
            Controller::GCC                       => StickType::Analog,
            Controller::Goomwave                  => StickType::Analog,
            Controller::Box                       => StickType::Digital,
            Controller::AnalogLStickDigitalCStick => StickType::Analog,
            Controller::Orca                      => StickType::Orca,
            Controller::Unknown                   => StickType::Unknown,
        }
    }
    
    pub const fn cstick(self) -> StickType {
        match self {
            Controller::GCC                       => StickType::Analog,
            Controller::Goomwave                  => StickType::Analog,
            Controller::Box                       => StickType::Digital,
            Controller::AnalogLStickDigitalCStick => StickType::Digital,
            Controller::Orca                      => StickType::Digital,
            Controller::Unknown                   => StickType::Unknown,
        }
    }
}

#[derive(Clone, Debug)]
pub struct PlayerViolations {
    /// Bitmask of which violations were checked for this player.
    /// 
    /// This will always be equal or a subset of `ViolationResult.checked`.
    /// The violations checked are based on the type of controller.
    pub checked: ViolationMask,
    
    /// Bitmask of which violations were detected for this player.
    pub found: ViolationMask,
    
    /// Whether the lstick is analog, digital, or orca.
    pub lstick_type: StickType,
    
    /// Whether the cstick is analog or digital.
    pub cstick_type: StickType,
    
    /// Guessed controller.
    pub controller: Controller,
    
    /// The percentage of control stick movements that do not have intermediate values.
    ///
    /// Between 0 and 1.
    /// Values less than 0.25 will count as a violation. 
    pub travel_time_hit_rate: f32,
    
    pub fuzz_rate: f32,
}

impl PlayerViolations {
    pub const DEFAULT: PlayerViolations = PlayerViolations {
        checked: 0,
        found: 0,
        lstick_type: StickType::Analog,
        cstick_type: StickType::Analog,
        controller: Controller::GCC,
        
        travel_time_hit_rate: 0.0,
        fuzz_rate: 0.0,
    };
}

#[derive(Clone, Debug)]
pub struct ViolationResult {
    /// Bitmask of which violations were checked in this game.
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
        res.checked |= violation_group::RAW_COORD_REQUIRED;
    } else {
        res.skipped |= violation_group::RAW_COORD_REQUIRED;
    }
    
    let mut stick_coords = Vec::new();
    let mut coord_set = HashSet::<(i8, i8)>::with_capacity(4096);
    
    for ply in 0..4 {
        let Some(ref frames) = game.frames[ply] else { continue; };
        let vio = &mut res.players[ply];
        
        // LSTICK CHECKS ---------------------------------------
        
        // push clamped lstick coords to buffer
        stick_coords.clear();
        stick_coords.reserve(frames.len());
        for f in frames {
            stick_coords.push(f.left_stick_coords_raw.clamped());
        }
        
        // determine digital/analog/orca lstick
        vio.lstick_type = lstick_type(&mut coord_set, &stick_coords);
        
        // determine checks for this player
        vio.checked |= res.checked & vio.lstick_type.lstick_checks();
        
        // travel time
        if vio.checked & violation::NO_TRAVEL_TIME != 0 {
            vio.travel_time_hit_rate = travel_time_hit_rate(&stick_coords);
            if vio.travel_time_hit_rate < 0.25 {
                vio.found |= violation::NO_TRAVEL_TIME;
            }
        }
            
        // coordinate fuzzing
        if vio.checked & violation::NO_FUZZING != 0 {
            vio.fuzz_rate = fuzz_rate(&stick_coords);
            
            // at least 50% of same-target inputs must be 1 off from target (25% to each side)
            if vio.fuzz_rate < 0.3 {
                vio.found |= violation::NO_FUZZING;
            }
        }
            
        // goomwave clamping
        if vio.checked & violation::ENLARGED_DEADZONES != 0 {
            if has_enlarged_deadzones(&stick_coords) {
                vio.found |= violation::ENLARGED_DEADZONES;
            }
        }
        
        // goomwave utilt rounding
        if vio.checked & violation::UTILT_ROUNDING != 0 {
            if has_utilt_rounding(&stick_coords) {
                vio.found |= violation::UTILT_ROUNDING;
            }
        }
        
        // banned lstick values
        if vio.checked & violation::ILLEGAL_LSTICK_COORDS != 0 {
            // TODO
        }
        
        // CSTICK CHECKS ---------------------------------------
        
        // push clamped cstick coords to buffer
        stick_coords.clear();
        for f in frames {
            stick_coords.push(f.right_stick_coords_raw.clamped());
        }
        
        // determine digital/analog/orca lstick
        vio.cstick_type = cstick_type(&mut coord_set, &stick_coords);
        
        // determine checks for this player
        vio.checked |= res.checked & vio.cstick_type.cstick_checks();
        
        // banned cstick values
        if vio.checked & violation::ILLEGAL_CSTICK_COORDS != 0 {
            // TODO
        }
        
        // GUESS CONTROLLER -------------------------------
        
        vio.controller = Controller::guess(
            vio.lstick_type, vio.cstick_type,
            vio.found,
        );
    }
    
    res
}

/// Only determines between analog and digital for now.
pub fn cstick_type(
    coord_set: &mut HashSet<(i8, i8)>,
    coords: &[VectorI8]
) -> StickType {
    coord_set.clear();
    
    for coord in coords {
        coord_set.insert((coord.x, coord.y));
    }
    
    // some sickos don't use it
    if coord_set.len() <= 1 { return StickType::Unknown; }
    
    if coord_set.len() < 10 {
        for (x, y) in coord_set.iter().copied() {
            let mut x = x as isize;
            let mut y = y as isize;
            
            // idk why but some box's csticks output weird, low numbers occasionally
            if x.abs() < 10 { x = 0; } 
            if y.abs() < 10 { y = 0; } 
            
            let dist_sq = x*x + y*y;
            if dist_sq != 0 && dist_sq < RIM_MIN_I {
                return StickType::Unknown;
            }
        }
        
        // digital if player only used cardinals with no travel time nerf
        return StickType::Digital;
    }
    
    let mut noncardinal = 0;
    let mut cardinal = 0;
    
    for (x, y) in coord_set.iter().copied() {
        if x == 0 || y == 0 { cardinal += 1; }
        else { noncardinal += 1; }
    }
    
    // oems have wayyyyy more noncardinal inputs than cardinal inputs
    if noncardinal > cardinal {
        StickType::Analog
    } else {
        StickType::Digital
    }
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

fn has_enlarged_deadzones(coords: &[VectorI8]) -> bool {
    // stick values [-6, 6] are set to zero on goomwaves
    
    for coord in coords {
        if coord.x != 0 && -7 < coord.x && coord.x < 7 { return false; }
        if coord.y != 0 && -7 < coord.y && coord.y < 7 { return false; }
    }
    true
}

fn fuzz_rate(coords: &[VectorI8]) -> f32 {
    let mut fuzzed_count = 0;
    let mut unfuzzed_count = 0;
    
    for group in coords.windows(2) {
        let [a, b] = group else { panic!(); };
        
        if a.x != 0 {
            match a.x.abs_diff(b.x) {
                0 => unfuzzed_count += 1,
                1|2 => fuzzed_count += 1,
                _ => ()
            }
        }
        
        if a.y != 0 {
            match a.y.abs_diff(b.y) {
                0 => unfuzzed_count += 1,
                1|2 => fuzzed_count += 1,
                _ => ()
            }
        }
    }
    
    if unfuzzed_count == 0 { return 1.0; }
    fuzzed_count as f32 / unfuzzed_count as f32
}

fn has_utilt_rounding(coords: &[VectorI8]) -> bool {
    let mut utilt_zone_count = 0;
    let mut utilt_boundary_count = 0;
    
    for coord in coords {
        if -22 <= coord.x && coord.x <= 22 {
            if 13 <= coord.y && coord.y <= 22 { return false; }
            
            if coord.y == 23 { utilt_boundary_count += 1; }
            if 23 <= coord.y && coord.y <= 52 { utilt_zone_count += 1; }
        }
    }
    
    let utilt_boundary_rate = utilt_boundary_count as f32 / utilt_zone_count as f32; 
    utilt_boundary_count > 5 && utilt_boundary_rate > 0.08
}

#[cfg(test)]
mod test {
    use super::*;

    #[track_caller]
    fn res(s: &str) -> ViolationResult {
        let (game, _) = slp_parser::read_game(std::path::Path::new(
            &format!("test_slps/{}", s)
        )).unwrap();
        let res = check_game(&game);
        assert_eq!(res.checked, violation_group::ALL);
        assert_eq!(res.skipped, 0);
        
        res
    }

    #[track_caller]
    fn box_no_nerf(pl: &PlayerViolations) {
        assert_eq!(pl.checked, Controller::Box.checks());
        assert_eq!(pl.found, violation::NO_TRAVEL_TIME | violation::NO_FUZZING);
        assert_eq!(pl.lstick_type, StickType::Digital);
        assert_eq!(pl.cstick_type, StickType::Digital);
        assert_eq!(pl.controller, Controller::Box);
    }
    
    #[track_caller]
    fn box_tt_nerf(pl: &PlayerViolations) {
        assert_eq!(pl.checked, Controller::Box.checks());
        assert_eq!(pl.found, violation::NO_FUZZING);
        assert_eq!(pl.lstick_type, StickType::Digital);
        assert_eq!(pl.cstick_type, StickType::Digital);
        assert_eq!(pl.controller, Controller::Box);
    }
    
    #[track_caller]
    fn gcc(pl: &PlayerViolations) {
        assert_eq!(pl.checked, Controller::GCC.checks());
        assert_eq!(pl.found, 0);
        assert_eq!(pl.lstick_type, StickType::Analog);
        assert_eq!(pl.cstick_type, StickType::Analog);
        assert_eq!(pl.controller, Controller::GCC);
    }
    
    #[track_caller]
    fn goomwave(pl: &PlayerViolations) {
        assert_eq!(pl.checked, Controller::Goomwave.checks());
        assert_eq!(pl.found, violation::ENLARGED_DEADZONES); // can't find utilt rounding test case
        assert_eq!(pl.lstick_type, StickType::Analog);
        assert_eq!(pl.cstick_type, StickType::Analog);
        assert_eq!(pl.controller, Controller::Goomwave);
    }
    
    #[track_caller]
    fn orca(pl: &PlayerViolations) {
        assert_eq!(pl.checked, Controller::Orca.checks());
        assert_eq!(pl.found, 0);
        assert_eq!(pl.lstick_type, StickType::Orca);
        assert_eq!(pl.cstick_type, StickType::Digital);
        assert_eq!(pl.controller, Controller::Orca);
    }
    
    #[test] fn goomwave_1() {
        let res = res("goomwave.slpz");
        gcc(&res.players[0]);
        goomwave(&res.players[1]);
    }
    
    #[test] fn goomwave_2() {
        let res = res("goomwave2.slpz");
        gcc(&res.players[0]);
        goomwave(&res.players[1]);
    }
    
    #[test] fn box_nonerf_1() {
        let res = res("p1_box_nonerf.slpz");
        box_no_nerf(&res.players[0]);
        gcc(&res.players[1]);
    }
    
    #[test] fn box_nonerf_2() {
        let res = res("p1_box_nonerf2.slpz");
        box_no_nerf(&res.players[0]);
        gcc(&res.players[1]);
    }
    
    #[test] fn box_nonerf_3() {
        let res = res("p1_box_nonerf3.slpz");
        box_no_nerf(&res.players[0]);
        gcc(&res.players[1]);
    }
    
    #[test] fn box_nonerf_4() {
        let res = res("p1_box_nonerf4.slpz");
        box_no_nerf(&res.players[0]);
        gcc(&res.players[1]);
    }
    
    #[test] fn box_tt_nerf_1() {
        let res = res("p1_box_nerf.slpz");
        box_tt_nerf(&res.players[0]);
        box_tt_nerf(&res.players[1]);
    }
    
    #[test] fn orca_1() {
        let res = res("p1_orca.slpz");
        orca(&res.players[0]);
        gcc(&res.players[1]);
    }
    
    #[test] fn orca_2() {
        let res = res("p1_orca2.slpz");
        orca(&res.players[0]);
        gcc(&res.players[1]);
    }
    
    #[test] fn orca_3() {
        let res = res("p1_orca3.slpz");
        orca(&res.players[0]);
        gcc(&res.players[1]);
    }
    
    #[test] fn orca_4() {
        let res = res("p1_orca4.slpz");
        orca(&res.players[0]);
        gcc(&res.players[1]);
    }
    
    #[test] fn orca_5() {
        let res = res("p1_orca5.slpz");
        orca(&res.players[0]);
        gcc(&res.players[1]);
    }
    
    #[test] fn oem_1() {
        let res = res("p1_oem.slpz");
        gcc(&res.players[0]);
        gcc(&res.players[1]);
    }
}
