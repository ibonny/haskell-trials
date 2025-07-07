#![allow(non_snake_case)]

use std::{thread, time::Duration};

use terminal_size::{Height, Width, terminal_size};

const DISTANCE_FROM_CAM: f64 = 100.0;
const K1: f64 = 40.0;
const BACKGROUND_ASCII_CODE: char = ' ';
const CUBE_WIDTH: f64 = 20.0;
const INCREMENT_SPEED: f64 = 0.6;
const HORIZONTAL_OFFSET: f64 = 0.05 * CUBE_WIDTH;

fn tsize() -> (u16, u16) {
    if let Some((Width(w), Height(h))) = terminal_size() {
        // Put a small box around the output.
        return (w - 2, h - 2);
    }

    return (0, 0);
}

fn calculate_x(i: f64, j: f64, k: f64, A: f64, B: f64, C: f64) -> f64 {
    return j * A.sin() * B.sin() * C.cos() - k * A.cos() * B.sin() * C.cos()
        + j * A.cos() * C.sin()
        + k * A.sin() * C.sin()
        + i * B.cos() * C.cos();
}

fn calculate_y(i: f64, j: f64, k: f64, A: f64, B: f64, C: f64) -> f64 {
    return j * A.cos() * C.cos() + k * A.sin() * C.cos() - j * A.sin() * B.sin() * C.sin()
        + k * A.cos() * B.sin() * C.sin()
        - i * B.cos() * C.sin();
}

fn calculate_z(i: f64, j: f64, k: f64, A: f64, B: f64) -> f64 {
    return k * A.cos() * B.cos() - j * A.sin() * B.cos() + i * B.sin();
}

fn calculate_for_surface(
    cube_x: f64,
    cube_y: f64,
    cube_z: f64,
    A: f64,
    B: f64,
    C: f64,
    ch: char,
    width: u16,
    height: u16,
    z_buffer: &mut Box<[f64]>,
    buffer: &mut Box<[char]>,
) {
    let x = calculate_x(cube_x, cube_y, cube_z, A, B, C);
    let y = calculate_y(cube_x, cube_y, cube_z, A, B, C);
    let z = calculate_z(cube_x, cube_y, cube_z, A, B) + DISTANCE_FROM_CAM;

    let ooz = 1.0 / z;

    let xp = (width as f64) / 2.0 + HORIZONTAL_OFFSET + K1 * ooz * x * 2.0;
    let yp = (height as f64) / 2.0 + K1 * ooz * y;

    let idx = (xp as u16) + (yp as u16) * width;

    // idx is always unsigned, so it's always position. No need to check >= 0.
    if idx < width * height {
        let index: usize = idx as usize;

        if ooz > z_buffer[index] {
            z_buffer[index] = ooz;
            buffer[index] = ch;
        }
    }
}

fn main() {
    let (width, height) = tsize();

    println!("We are looking at: {} x {}", width, height);

    let dimens: usize = (width * height).into();

    let mut buffer = vec![' '; dimens].into_boxed_slice();
    let mut z_buffer = vec![0f64; dimens].into_boxed_slice();

    let (mut A, mut B, mut C) = (0.0, 0.0, 0.0);

    print!("\x1b[2J");

    loop {
        for i in 0..buffer.len() {
            buffer[i] = BACKGROUND_ASCII_CODE;
        }

        for i in 0..z_buffer.len() {
            z_buffer[i] = 0.0;
        }

        let cubeWidth = 20.0;

        // first cube
        let mut cubeX = -cubeWidth;

        while cubeX < cubeWidth {
            let mut cubeY = -cubeWidth;

            while cubeY < cubeWidth {
                // fn calculate_for_surface(cubeX: f64, cubeY: f64, cubeZ: f64, A: f64, B: f64, C: f64, ch: char, width: f64, height: f64,
                // z_buffer: &mut Box<[f64]>, buffer: &mut Box<[char]>)

                calculate_for_surface(
                    cubeX,
                    cubeY,
                    -cubeWidth,
                    A,
                    B,
                    C,
                    '@',
                    width,
                    height,
                    &mut z_buffer,
                    &mut buffer,
                );
                calculate_for_surface(
                    cubeWidth,
                    cubeY,
                    cubeX,
                    A,
                    B,
                    C,
                    '$',
                    width,
                    height,
                    &mut z_buffer,
                    &mut buffer,
                );
                calculate_for_surface(
                    -cubeWidth,
                    cubeY,
                    -cubeX,
                    A,
                    B,
                    C,
                    '%',
                    width,
                    height,
                    &mut z_buffer,
                    &mut buffer,
                );
                calculate_for_surface(
                    -cubeX,
                    cubeY,
                    cubeWidth,
                    A,
                    B,
                    C,
                    '#',
                    width,
                    height,
                    &mut z_buffer,
                    &mut buffer,
                );
                calculate_for_surface(
                    cubeX,
                    -cubeWidth,
                    -cubeY,
                    A,
                    B,
                    C,
                    ';',
                    width,
                    height,
                    &mut z_buffer,
                    &mut buffer,
                );
                calculate_for_surface(
                    cubeX,
                    cubeWidth,
                    cubeY,
                    A,
                    B,
                    C,
                    '+',
                    width,
                    height,
                    &mut z_buffer,
                    &mut buffer,
                );

                cubeY = cubeY + INCREMENT_SPEED;
            }

            cubeX = cubeX + INCREMENT_SPEED;
        }

        print!("\x1b[H");

        for k in 0..(width * height) {
            if k % width == 0 {
                println!("");
            } else {
                print!("{}", buffer[k as usize]);
            }
        }

        // process::exit(0);

        A += 0.05;
        B += 0.05;
        C += 0.05;

        thread::sleep(Duration::from_micros(8000 * 10));
    }
}