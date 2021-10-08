#pragma once

#include <cassert>
#include <cstdlib>
#include <new>
#include <utility>
#include <memory>
#include <algorithm>

template <typename T>
class RawMemory {
public:
    RawMemory() = default;

    explicit RawMemory(size_t capacity) : m_buffer(Allocate(capacity)), m_capacity(capacity) { }

    ~RawMemory() {
        Deallocate(m_buffer);
    }

    RawMemory(const RawMemory&) = delete;
    RawMemory& operator=(const RawMemory& rhs) = delete;

    RawMemory(RawMemory&& other) noexcept {
        Swap(other);
    }

    RawMemory& operator=(RawMemory&& rhs) noexcept {
        Swap(rhs);
        return *this;
    }

    T* operator+(size_t offset) noexcept {
        assert(offset <= m_capacity);
        return m_buffer + offset;
    }

    const T* operator+(size_t offset) const noexcept {
        return const_cast<RawMemory&>(*this) + offset;
    }

    const T& operator[](size_t index) const noexcept {
        return const_cast<RawMemory&>(*this)[index];
    }

    T& operator[](size_t index) noexcept {
        assert(index < m_capacity);
        return m_buffer[index];
    }

    void Swap(RawMemory& other) noexcept {
        std::swap(m_buffer, other.m_buffer);
        std::swap(m_capacity, other.m_capacity);
    }

    const T* GetAddress() const noexcept {
        return m_buffer;
    }

    T* GetAddress() noexcept {
        return m_buffer;
    }

    size_t Capacity() const {
        return m_capacity;
    }

    const T* Begin() const {
        return m_buffer;
    }
    T* Begin() {
        return m_buffer;
    }

private:
    T* m_buffer = nullptr;
    size_t m_capacity = 0;

    static T* Allocate(size_t n) {
        return n != 0 ? static_cast<T*>(operator new(n * sizeof(T))) : nullptr;
    }

    static void Deallocate(T* buf) noexcept {
        operator delete(buf);
    }
};

template <typename T>
class Vector {
public:
    Vector() = default;

    explicit Vector(size_t size) : m_data(size), m_size(size) {
        std::uninitialized_value_construct_n(m_data.GetAddress(), size);
    }

    Vector(const Vector& other) : m_data(other.m_size), m_size(other.m_size) {
        std::uninitialized_copy_n(other.m_data.GetAddress(), other.m_size, m_data.GetAddress());
    }

    Vector(Vector&& other) noexcept {
        Swap(other);
    }

    Vector& operator=(const Vector& rhs) {
        if (this == &rhs) return *this;

        if (rhs.m_size > m_data.Capacity()) {
            Vector<T> t(rhs);
            Swap(t);
        }
        else {
            if (m_size < rhs.m_size) {
                std::uninitialized_copy_n(rhs.m_data.GetAddress() + m_size, rhs.m_size - m_size, m_data.GetAddress() + m_size);
                std::copy(rhs.m_data.GetAddress(), rhs.m_data.GetAddress() + m_size, m_data.GetAddress());
            }

            if (m_size > rhs.m_size) {
                std::destroy_n(m_data.GetAddress() + rhs.m_size, m_size - rhs.m_size);
                std::copy(rhs.m_data.GetAddress(), rhs.m_data.GetAddress() + rhs.m_size, m_data.GetAddress());
            }
            m_size = rhs.m_size;
        }
        return *this;
    }

    Vector& operator=(Vector&& rhs) noexcept {
        Swap(rhs);
        return *this;
    }

    void Swap(Vector& other) noexcept {
        m_data.Swap(other.m_data);
        std::swap(m_size, other.m_size);
    }

    void Reserve(size_t new_capacity) {
        if (new_capacity <= m_data.Capacity()) return;
        RawMemory<T> new_data(new_capacity);
        if constexpr (std::is_nothrow_move_constructible_v<T> || !std::is_copy_constructible_v<T>) {
            std::uninitialized_move_n(m_data.GetAddress(), m_size, new_data.GetAddress());
        }
        else {
            std::uninitialized_copy_n(m_data.GetAddress(), m_size, new_data.GetAddress());
        }
        std::destroy_n(m_data.GetAddress(), m_size);
        m_data.Swap(new_data);
    }

    ~Vector() {
        std::destroy_n(m_data.GetAddress(), m_size);
    }

    size_t Size() const noexcept {
        return m_size;
    }

    size_t Capacity() const noexcept {
        return m_data.Capacity();
    }

    const T& operator[](size_t index) const noexcept {
        return const_cast<Vector&>(*this)[index];
    }

    T& operator[](size_t index) noexcept {
        assert(index < m_size);
        return m_data[index];
    }

    void Resize(size_t new_size) {
        Reserve(new_size);
        if (m_size < new_size) {
            std::uninitialized_value_construct_n(m_data + m_size, new_size - m_size);
        }
        else if (m_size > new_size) {
            std::destroy_n(m_data + new_size, m_size - new_size);
        }
        m_size = new_size;
    }

    template <typename Type>
    void PushBack(Type&& value) {
        EmplaceBack(std::forward<Type>(value));
    }

    void PopBack() noexcept {
        std::destroy_at(m_data + m_size - 1);
        --m_size;
    }

    template <typename... Args>
    T& EmplaceBack(Args&&... args) {
        if (m_data.Capacity() > Size()) {
            auto tmp = new (m_data.GetAddress() + Size()) T(std::forward<Args>(args)...);
            ++m_size;
            return *tmp;
        }
        else {
            RawMemory<T> new_data(Size() == 0 ? 1 : Size() * 2);
            auto tmp = new (new_data.GetAddress() + Size()) T(std::forward<Args>(args)...);
            if constexpr (std::is_nothrow_move_constructible_v<T> || !std::is_copy_constructible_v<T>) {
                std::uninitialized_move_n(m_data.GetAddress(), Size(), new_data.GetAddress());
            }
            else {
                std::uninitialized_copy_n(m_data.GetAddress(), Size(), new_data.GetAddress());
            }
            std::destroy_n(m_data.GetAddress(), Size());
            m_data.Swap(new_data);
            ++m_size;
            return *tmp;
        }
    }

    using iterator = T*;
    using const_iterator = const T*;

    iterator begin() noexcept {
        return m_data.Begin();
    }

    iterator end() noexcept {
        return m_data + m_size;
    }

    const_iterator begin() const noexcept {
        return m_data.Begin();
    }

    const_iterator end() const noexcept {
        return m_data + m_size;
    }

    const_iterator cbegin() const noexcept {
        return m_data.Begin();
    }

    const_iterator cend() const noexcept {
        return m_data + m_size;
    }

    template <typename... Args>
    iterator Emplace(const_iterator pos, Args&&... args) {
        const size_t offset = pos - m_data.GetAddress();
        if (m_data.Capacity() > Size()) {
            BigCapacity(pos, std::forward<Args>(args)...);
        }
        else {
            CompletelyFilled(pos, std::forward<Args>(args)...);
        }
        return begin() + offset;
    }

    iterator Erase(const_iterator pos) {
        if (pos == end()) {
            return end();
        }
        auto position_elemet = begin() + (pos - cbegin());
        std::move(position_elemet + 1, end(), position_elemet);
        std::destroy_at(end() - 1);
        --m_size;
        return position_elemet;
    }

    iterator Insert(const_iterator pos, const T& value) {
        return Emplace(pos, value);
    }

    iterator Insert(const_iterator pos, T&& value) {
        return Emplace(pos, std::move(value));

    }

private:
    static void DestroyN(T* buf, size_t n) noexcept {
        for (size_t i = 0; i != n; ++i) {
            Destroy(buf + i);
        }
    }

    static void CopyConstruct(T* buf, const T& elem) {
        new (buf) T(elem);
    }

    static void Destroy(T* buf) noexcept {
        buf->~T();
    }

    template <typename... Args>
    void BigCapacity(const_iterator pos, Args&&... args) {
        const size_t offset = pos - m_data.GetAddress();
        auto position_elemet = begin() + offset;
        if (position_elemet == end()) {
            EmplaceBack(std::forward<Args>(args)...);
        }
        else {
            T tmp_new_elem{ std::forward<Args>(args)... };
            new (m_data + m_size) T(std::move(*(end() - 1)));
            std::move_backward(position_elemet, end() - 1, m_data + m_size);
            *position_elemet = std::move(tmp_new_elem);
            ++m_size;
        }
    }

    template <typename... Args>
    void CompletelyFilled(const_iterator pos, Args&&... args) {
        const size_t offset = pos - m_data.GetAddress();
        RawMemory<T> new_data(Size() == 0 ? 1 : Size() * 2);
        if constexpr (std::is_nothrow_move_constructible_v<T> || !std::is_copy_constructible_v<T>) {
            std::uninitialized_move_n(m_data.GetAddress(), offset, new_data.GetAddress());
        }
        else {
            try {
                std::uninitialized_copy_n(m_data.GetAddress(), offset, new_data.GetAddress());
            }
            catch (...) {
                std::destroy_n(new_data + offset, 1);
                throw;
            }
        }
        new (new_data + offset) T(std::forward<Args>(args)...);
        if (m_size > offset) {
            if constexpr (std::is_nothrow_move_constructible_v<T> || !std::is_copy_constructible_v<T>) {
                std::uninitialized_move_n(
                    m_data.GetAddress() + offset,
                    m_size - offset,
                    new_data.GetAddress() + offset + 1);
            }
            else {
                try {
                    std::uninitialized_copy_n(
                        m_data.GetAddress() + offset,
                        m_size - offset,
                        new_data.GetAddress() + offset + 1);
                }
                catch (...) {
                    std::destroy_n(new_data.GetAddress(), offset + 1);
                    throw;
                }

            }
        }
        std::destroy_n(m_data.GetAddress(), Size());
        m_data.Swap(new_data);
        ++m_size;
    }

private:
    RawMemory<T> m_data;
    size_t m_size = 0;
};