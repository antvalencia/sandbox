import matplotlib.pyplot as plt

x = [2,3,4,5,6]
y = [
    21.798557984538988,
    11.927180213371985,
    9.078933639372583,
    8.267036504165342,
    6.006964022367253
]
plt.xlabel('k')
plt.ylabel('Within cluster sum of squared error')
plt.title('Within cluster sum of squared error (vs) k')
plt.plot(x, y, 'k', x, y, 'bo')
plt.axis([0, 8, 0, 25])
plt.grid(True)
plt.show()
