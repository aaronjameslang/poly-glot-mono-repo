// TODO this is my algortihm but I'll admit I don't don't super
// understand the rust stuff, especially the iterators vs enumerators
// and all the borrowing stuff.
// I'd like to use reduce rather than fold to simplify the code but
// I can't figure out how to make the types work out for now.
// I'd also like to pass step to fold directly

pub fn longest_slide_down(pyramid: &[Vec<i32>]) -> i32 {
    // Create a copy of the pyramid that we can modify
    let mut rows: Vec<Vec<i32>> = pyramid.to_vec();
    rows.reverse();

    let result = rows
        .iter()
        .skip(1) // Skip the first row since it's our initial accumulator
        .fold(rows[0].clone(), |acc, row| step(&acc, row));
    result[0]
}

fn step(arr_b: &[i32], arr_a: &[i32]) -> Vec<i32> {
    arr_a
        .iter()
        .enumerate()
        .map(|(i, &val)| val + std::cmp::max(arr_b[i], arr_b[i + 1]))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_small() {
        #[rustfmt::skip]
        let small = vec![
            vec![3],
            vec![7, 4],
            vec![2, 4, 6],
            vec![8, 5, 9, 3]
        ];
        assert_eq!(
            longest_slide_down(&small),
            23,
            "It should work for small pyramids"
        );
    }

    #[test]
    fn test_small_tricky() {
        let small = vec![
            vec![10],
            vec![10, 20],
            vec![10, 10, 20],
            vec![10, 90, 10, 20],
        ];
        assert_eq!(
            longest_slide_down(&small),
            130,
            "It should work for small, tricky pyramids"
        );
    }

    #[test]
    fn test_medium() {
        let medium = vec![
            vec![75],
            vec![95, 64],
            vec![17, 47, 82],
            vec![18, 35, 87, 10],
            vec![20, 4, 82, 47, 65],
            vec![19, 1, 23, 75, 3, 34],
            vec![88, 2, 77, 73, 7, 63, 67],
            vec![99, 65, 4, 28, 6, 16, 70, 92],
            vec![41, 41, 26, 56, 83, 40, 80, 70, 33],
            vec![41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
            vec![53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
            vec![70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
            vec![91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
            vec![63, 66, 4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
            vec![4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 4, 23],
        ];
        assert_eq!(
            longest_slide_down(&medium),
            1074,
            "It should work for medium pyramids"
        );
    }
}
