from setuptools import setup

setup(name='taiji-utils',
      version='0.2.0',
      description='Taiji pipeline',
      url='https://taiji-pipeline.github.io/', 
      author='Kai Zhang',
      author_email='kai@kzhang.org',
      license='MIT',
      packages=['taiji_utils'],
      entry_points = {
        'console_scripts': ['taiji-utils=taiji_utils.__init__:main'],
      },
      install_requires=[
          'scipy',
          'numpy',
          'scikit-learn',
          'python-igraph',
          'statsmodels>=0.12.1',
          'umap-learn',
          'leidenalg',
          'scrublet',
          'csaps',
      ],
      zip_safe=False)
