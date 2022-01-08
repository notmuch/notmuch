import collections.abc

import pytest

import notmuch2._database as dbmod

import notmuch2._config as config


class TestIter:

    @pytest.fixture
    def db(self, maildir):
        with dbmod.Database.create(maildir.path) as db:
            yield db

    def test_type(self, db):
        assert isinstance(db.config, collections.abc.MutableMapping)
        assert isinstance(db.config, config.ConfigMapping)

    def test_alive(self, db):
        assert db.config.alive

    def test_set_get(self, maildir):
        # Ensure get-set works from different db objects
        with dbmod.Database.create(maildir.path, config=dbmod.Database.CONFIG.EMPTY) as db0:
            db0.config['spam'] = 'ham'
        with dbmod.Database(maildir.path, config=dbmod.Database.CONFIG.EMPTY) as db1:
            assert db1.config['spam'] == 'ham'

    def test_get_keyerror(self, db):
        with pytest.raises(KeyError):
            val = db.config['not-a-key']
            print(repr(val))

    def test_iter(self, db):
        assert list(db.config) == []
        db.config['spam'] = 'ham'
        db.config['eggs'] = 'bacon'
        assert set(db.config) == {'spam', 'eggs'}
        assert set(db.config.keys()) == {'spam', 'eggs'}
        assert set(db.config.values()) == {'ham', 'bacon'}
        assert set(db.config.items()) == {('spam', 'ham'), ('eggs', 'bacon')}

    def test_len(self, db):
        assert len(db.config) == 0
        db.config['spam'] = 'ham'
        assert len(db.config) == 1
        db.config['eggs'] = 'bacon'
        assert len(db.config) == 2

    def test_del(self, db):
        db.config['spam'] = 'ham'
        assert db.config.get('spam') == 'ham'
        del db.config['spam']
        assert db.config.get('spam') is None
