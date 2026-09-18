use crate::{fail, list};

const MAX_ROUNDS: usize = 100;

#[unsafe(no_mangle)]
#[allow(non_snake_case)]
pub unsafe extern "C" fn englang_kMeans(data: *const f64, k: i64) -> *mut i64 {
    let points = unsafe { list::as_slice(data) };
    list::from_slice(&k_means(points, k))
}

fn k_means(points: &[f64], k: i64) -> Vec<i64> {
    if k < 1 {
        fail(&format!("kMeans needs at least 1 group, but was asked for {}.", k));
    }
    let k = k as usize;
    if k > points.len() {
        fail(&format!(
            "kMeans can't split {} numbers into {} groups. Ask for {} groups or fewer.",
            points.len(),
            k,
            points.len()
        ));
    }

    // Spreading the starting centers across the sorted data makes results
    // deterministic, which matters more for teaching than random restarts.
    let mut sorted = points.to_vec();
    sorted.sort_by(f64::total_cmp);
    let mut centers: Vec<f64> = (0..k)
        .map(|i| sorted[(2 * i + 1) * sorted.len() / (2 * k)])
        .collect();

    let mut assignments = vec![0i64; points.len()];
    for round in 0..MAX_ROUNDS {
        let mut changed = false;
        for (point, assignment) in points.iter().zip(assignments.iter_mut()) {
            let nearest = nearest_center(*point, &centers);
            if round == 0 || nearest != *assignment {
                changed = true;
                *assignment = nearest;
            }
        }
        if !changed {
            break;
        }

        let mut sums = vec![0.0; k];
        let mut counts = vec![0usize; k];
        for (point, assignment) in points.iter().zip(&assignments) {
            sums[*assignment as usize] += point;
            counts[*assignment as usize] += 1;
        }
        for i in 0..k {
            if counts[i] > 0 {
                centers[i] = sums[i] / counts[i] as f64;
            }
        }
    }
    assignments
}

fn nearest_center(point: f64, centers: &[f64]) -> i64 {
    let mut best = 0;
    for i in 1..centers.len() {
        if (point - centers[i]).abs() < (point - centers[best]).abs() {
            best = i;
        }
    }
    best as i64
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn separates_two_clear_groups() {
        assert_eq!(k_means(&[1.0, 1.5, 2.0, 8.0, 8.5, 9.0], 2), [0, 0, 0, 1, 1, 1]);
    }

    #[test]
    fn uses_middle_clusters() {
        let groups = k_means(&[0.0, 0.1, 5.0, 5.1, 10.0, 10.1], 3);
        assert_eq!(groups, [0, 0, 1, 1, 2, 2]);
    }

    #[test]
    fn moves_centers_away_from_a_bad_start() {
        let groups = k_means(&[1.0, 2.0, 3.0, 4.0, 100.0], 2);
        assert_eq!(groups, [0, 0, 0, 0, 1]);
    }

    #[test]
    fn one_group_holds_everything() {
        assert_eq!(k_means(&[3.0, -1.0, 7.0], 1), [0, 0, 0]);
    }

    #[test]
    fn round_trips_through_the_list_layout() {
        let data = list::from_slice(&[1.0, 1.2, 9.0, 9.4]);
        let groups = unsafe { list::as_slice(englang_kMeans(data, 2)) };
        assert_eq!(groups, [0, 0, 1, 1]);
    }
}
