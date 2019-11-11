from setuptools import setup

setup(name='taiji-utils',
      version='0.1',
      description='Taiji pipeline',
      url='https://taiji-pipeline.github.io/', 
      author='Kai Zhang',
      author_email='kai@kzhang.org',
      license='MIT',
      packages=['taiji_utils'],
      entry_points = {
        'console_scripts': ['taiji-utils=taiji_utils.command_line:main'],
      },
      install_requires=[
          'scipy',
          'numpy',
          'scikit-learn',
          'python-igraph',
          'umap-learn',
          'leidenalg',
          'MulticoreTSNE',
          'scrublet',
      ],
      zip_safe=False)