
#include <iostream>
#include <iomanip>
#include <chrono>

// Function to perform the calculation
double calculate(long iterations, double param1, double param2) {
    double result = 1.0;
    double j;
    for (long i = 1; i <= iterations; ++i) {
        j = i * param1 - param2;
        result -= (1.0 / j);
        j = i * param1 + param2;
        result += (1.0 / j);
    }
    return result;
}

int main() {
    // Timing and calculations
    auto start_time = std::chrono::high_resolution_clock::now();
    double result = calculate(100000000, 4.0, 1.0) * 4;
    auto end_time = std::chrono::high_resolution_clock::now();

    // Output results
    std::chrono::duration<double> elapsed = end_time - start_time;
    std::cout << "Result: " << std::fixed << std::setprecision(12) << result << std::endl;
    std::cout << "Execution Time: " << std::fixed << std::setprecision(6) << elapsed.count() << " seconds" << std::endl;

    return 0;
}
